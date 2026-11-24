/* global chrome */
(function(root) {
    "use strict";

    // Reviewed against pinned Playwright 1.65.1 coreBundle noDefaults initialization.
    // No viewport/window emulation: screenshots requiring a metrics override remain denied.
    const MAX_BODY = 256 * 1024;
    const MAX_CAPTURE = 128;
    const FOCUS_TIMEOUT_MS = 2000;
    const RENDER_TIMEOUT_MS = 2000;
    const CAPTURE_TIMEOUT_MS = 6500;
    const MAX_SCREENSHOT_PIXELS = 32 * 1024 * 1024;
    const MAX_SCREENSHOT_BASE64 = 4 * Math.ceil(500000 / 3);
    const PAGE_COMMANDS = new Set([
        "Page.enable", "Page.disable", "Page.getFrameTree", "Page.getLayoutMetrics",
        "Page.setLifecycleEventsEnabled", "Page.createIsolatedWorld",
        "Page.addScriptToEvaluateOnNewDocument", "Page.removeScriptToEvaluateOnNewDocument",
        "Page.navigate", "Page.reload", "Page.getNavigationHistory", "Page.navigateToHistoryEntry",
        "Page.captureScreenshot", "Page.handleJavaScriptDialog",
        "Runtime.enable", "Runtime.disable", "Runtime.evaluate", "Runtime.callFunctionOn",
        "Runtime.getProperties", "Runtime.releaseObject", "Runtime.releaseObjectGroup",
        "Runtime.addBinding", "Runtime.removeBinding", "Runtime.runIfWaitingForDebugger",
        "DOM.enable", "DOM.disable", "DOM.getDocument", "DOM.getFlattenedDocument",
        "DOM.describeNode", "DOM.resolveNode", "DOM.requestNode", "DOM.requestChildNodes",
        "DOM.querySelector", "DOM.querySelectorAll", "DOM.getAttributes", "DOM.getOuterHTML",
        "DOM.getBoxModel", "DOM.getContentQuads", "DOM.getNodeForLocation", "DOM.getFrameOwner",
        "DOM.scrollIntoViewIfNeeded", "DOM.focus",
        "CSS.enable", "CSS.disable", "CSS.getComputedStyleForNode", "CSS.getMatchedStylesForNode",
        "CSS.getInlineStylesForNode", "CSS.getStyleSheetText", "CSS.getMediaQueries",
        "Accessibility.enable", "Accessibility.disable", "Accessibility.getFullAXTree",
        "Accessibility.getPartialAXTree", "Accessibility.getRootAXNode",
        "Accessibility.getAXNodeAndAncestors", "Accessibility.getChildAXNodes",
        "Accessibility.queryAXTree",
        "Input.dispatchKeyEvent", "Input.dispatchMouseEvent", "Input.insertText",
        "Input.dispatchTouchEvent", "Input.dispatchDragEvent",
        "Network.enable", "Network.disable", "Network.getResponseBody",
        "Network.getRequestPostData", "Network.searchInResponseBody",
        "Log.enable", "Log.disable", "Inspector.enable", "Inspector.disable",
        "Emulation.setFocusEmulationEnabled", "Emulation.setEmulatedMedia",
        "Emulation.setDefaultBackgroundColorOverride",
    ]);
    const EVENT_DOMAINS = new Set([
        "Page", "Runtime", "DOM", "CSS", "Accessibility", "Network", "Log", "Inspector",
    ]);
    const FRAME_FILTER = [{type: "iframe", exclude: false}, {exclude: true}];
    const copy = value => JSON.parse(JSON.stringify(value));

    class AuthorizedCdpBridge {
        constructor({chrome, tabId, checkAuthorization, send, onError}) {
            if (!Number.isInteger(tabId) || typeof checkAuthorization !== "function" ||
                typeof send !== "function" || !chrome?.debugger) {
                throw new Error("AuthorizedCdpBridge requires an exact tab and authorization/transport");
            }
            this.chrome = chrome;
            this.tabId = tabId;
            // Synchronous authority check (return false or throw to revoke); no async check-to-send gap.
            this.checkAuthorization = checkAuthorization;
            this.send = send;
            this.onError = onError || (error => console.error("AuthorizedCdpBridge:", error));
            this.generation = 0;
            this.closed = false;
            this.rootSessionId = (root.crypto || require("node:crypto").webcrypto).randomUUID();
            this.sessions = new Map();
            this.bodies = new Map();
            this.retiredSessions = new Set();
            this.targetInfo = null;
            this.bootstrap = null;
            this.rootAttachment = null;
            this.capture = null;
            this.lastCaptureFrameId = 0;
            this.buffer = [];
            this.eventListener = (source, method, params) => {
                try { this.event(source, method, copy(params || {})); }
                catch (error) { this.report(error); }
            };
            this.detachListener = (source, reason) => {
                if (source.tabId !== this.tabId || source.sessionId || this.closed) return;
                this.close();
                this.report(new Error(`Authorized debugger detached: ${reason}`));
            };
            chrome.debugger.onEvent.addListener(this.eventListener);
            chrome.debugger.onDetach.addListener(this.detachListener);
        }

        report(error) {
            this.onError(error instanceof Error ? error : new Error(String(error)));
        }

        authorized(generation = this.generation) {
            if (this.closed || generation !== this.generation) return false;
            try {
                const result = this.checkAuthorization();
                if (result && typeof result.then === "function") {
                    // Observe rejection without accepting asynchronous authority checks.
                    Promise.resolve(result).catch(error => this.report(error));
                    throw new Error("checkAuthorization must be synchronous");
                }
                if (result === false) throw new Error("CDP authorization revoked");
                if (this.closed || generation !== this.generation) return false;
                return true;
            } catch (error) {
                this.close();
                this.report(error);
                return false;
            }
        }

        emit(message, generation = this.generation) {
            if (!this.authorized(generation)) return;
            try {
                const pending = this.send(message);
                if (pending && typeof pending.catch === "function") {
                    pending.catch(error => { this.close(); this.report(error); });
                }
            } catch (error) {
                this.close();
                this.report(error);
            }
        }

        session(id) {
            const session = this.sessions.get(id);
            if (!session) throw new Error("Unknown or stale CDP session");
            return session;
        }

        async command(session, method, params, generation) {
            if (!this.authorized(generation)) throw new Error("CDP authorization revoked");
            if (session && this.sessions.get(session.id) !== session) {
                throw new Error("Unknown or stale CDP session");
            }
            const source = {tabId: this.tabId};
            if (session && session.id !== this.rootSessionId) source.sessionId = session.id;
            const result = await this.chrome.debugger.sendCommand(source, method, params);
            if (!this.authorized(generation)) throw new Error("CDP authorization revoked");
            if (session && this.sessions.get(session.id) !== session) {
                throw new Error("CDP session detached during command");
            }
            return result || {};
        }

        async exactTarget(generation) {
            if (!this.bootstrap) {
                const pending = this.command(null, "Target.getTargetInfo", {}, generation).then(result => {
                    if (!this.authorized(generation)) throw new Error("CDP authorization revoked");
                    const info = result.targetInfo;
                    if (!info || info.type !== "page" || typeof info.targetId !== "string" ||
                        !info.targetId || (this.targetInfo &&
                        info.targetId !== this.targetInfo.targetId)) {
                        throw new Error("Exact tab did not resolve to the authorized page target");
                    }
                    this.targetInfo = copy(info);
                    return this.targetInfo;
                });
                this.bootstrap = pending;
            }
            const pending = this.bootstrap;
            try {
                const info = await pending;
                if (!this.authorized(generation)) throw new Error("CDP authorization revoked");
                return info;
            } catch (error) {
                this.buffer = [];
                throw error;
            } finally {
                if (this.bootstrap === pending) this.bootstrap = null;
            }
        }

        async attachRoot(generation) {
            if (this.sessions.has(this.rootSessionId)) return;
            if (!this.rootAttachment) {
                this.rootAttachment = (async () => {
                    const info = await this.exactTarget(generation);
                    if (!this.authorized(generation)) throw new Error("CDP authorization revoked");
                    let timer;
                    try {
                        // noDefaults skips Playwright's focus emulation; hidden tabs can drop trusted input.
                        await Promise.race([
                            this.command(null, "Emulation.setFocusEmulationEnabled", {enabled: true}, generation),
                            new Promise((_, reject) => { timer = setTimeout(() =>
                                reject(Object.assign(new Error("Exact-tab focus preparation timed out"),
                                    {retireBridge: true})), FOCUS_TIMEOUT_MS); }),
                        ]);
                    } finally { clearTimeout(timer); }
                    if (!this.authorized(generation)) throw new Error("CDP authorization revoked");
                    this.sessions.set(this.rootSessionId, {id: this.rootSessionId,
                        parent: null, targetId: info.targetId});
                    this.emit({method: "Target.attachedToTarget", params: {
                        sessionId: this.rootSessionId, targetInfo: copy(info), waitingForDebugger: false,
                    }}, generation);
                    const buffered = this.buffer;
                    this.buffer = [];
                    for (const event of buffered) this.event(...event);
                })();
            }
            const pending = this.rootAttachment;
            try { await pending; }
            catch (error) { this.buffer = []; throw error; }
            finally { if (this.rootAttachment === pending) this.rootAttachment = null; }
        }

        validateAutoAttach(params) {
            if (params.autoAttach !== true || params.flatten !== true ||
                typeof params.waitForDebuggerOnStart !== "boolean" ||
                Object.keys(params).some(key =>
                    !["autoAttach", "flatten", "waitForDebuggerOnStart", "filter"].includes(key)) ||
                (params.filter !== undefined &&
                JSON.stringify(params.filter) !== JSON.stringify(FRAME_FILTER))) {
                throw new Error("Only flattened iframe-only auto attachment is permitted");
            }
        }

        async captureScreenshot(session, params, generation) {
            if (session.id !== this.rootSessionId) throw new Error("Screenshots require the exact root page");
            if (this.capture) throw new Error("Screenshot capture already in progress");
            const clip = params.clip;
            if (params.format !== "png" || Object.keys(params).some(key =>
                    !["format", "clip", "captureBeyondViewport"].includes(key)) ||
                    typeof params.captureBeyondViewport !== "boolean" || !clip ||
                    Object.keys(clip).sort().join(",") !== "height,scale,width,x,y" ||
                    ![clip.x, clip.y, clip.width, clip.height, clip.scale].every(Number.isFinite) ||
                    clip.x < 0 || clip.y < 0 || clip.width <= 0 || clip.height <= 0 || clip.scale <= 0 ||
                    clip.width > 32768 || clip.height > 32768 || clip.scale > 4 ||
                    clip.width * clip.height * clip.scale * clip.scale > MAX_SCREENSHOT_PIXELS) {
                throw new Error("Unsupported or oversized screenshot geometry");
            }
            let resolve, reject, rejectFrame, rejectFailure;
            const frame = new Promise((yes, no) => { resolve = yes; rejectFrame = no; });
            const failure = new Promise((_, no) => { rejectFailure = no; });
            frame.catch(() => {}); failure.catch(() => {});
            reject = error => { rejectFrame(error); rejectFailure(error); };
            const capture = {session, generation, resolve, reject, frame, failure, pendingFrames: [],
                ackDraining: false, ack: Promise.resolve(), frameId: null, started: performance.now()};
            this.capture = capture;
            const bounded = async (label, milliseconds, task) => {
                let timer;
                try {
                    const remaining = Math.min(milliseconds, 9000 - (performance.now() - capture.started));
                    if (remaining <= 0) throw new Error(`${label} timed out`);
                    return await Promise.race([Promise.resolve().then(task), failure, new Promise((_, no) => {
                        timer = setTimeout(() => no(new Error(`${label} timed out`)), remaining);
                    })]);
                } finally { clearTimeout(timer); }
            };
            try {
                let captureParams = params;
                let pageZoom;
                await bounded("Screenshot layout admission", RENDER_TIMEOUT_MS, async () => {
                    const metrics = await this.command(session, "Page.getLayoutMetrics", {}, generation);
                    pageZoom = metrics.cssVisualViewport?.zoom ?? 1;
                    if (!Number.isFinite(pageZoom) || pageZoom <= 0) throw new Error("Invalid screenshot page zoom");
                    // Playwright supplies CSS coordinates; Chromium's non-emulated screenshot clip uses DIPs.
                    captureParams = {...params, clip: {...clip,
                        x: clip.x * pageZoom, y: clip.y * pageZoom,
                        width: clip.width * pageZoom, height: clip.height * pageZoom}};
                    const deviceWidth = metrics.contentSize?.width;
                    const cssWidth = metrics.cssContentSize?.width;
                    const factor = deviceWidth / cssWidth;
                    const width = Math.ceil(clip.width * clip.scale * factor);
                    const height = Math.ceil(clip.height * clip.scale * factor);
                    if (captureParams.clip.width > 32768 || captureParams.clip.height > 32768 ||
                            !Number.isFinite(factor) || factor <= 0 || !Number.isSafeInteger(width) ||
                            !Number.isSafeInteger(height) || width <= 0 || height <= 0 ||
                            width > 32768 || height > 32768 || width * height > MAX_SCREENSHOT_PIXELS) {
                        throw new Error("Screenshot exceeds the device-pixel geometry limit");
                    }
                });
                // A tiny private frame producer keeps the inactive view's compositor surface alive.
                // Its JPEG frames are discarded; only Playwright's original PNG command is returned.
                await bounded("Screenshot render preparation", RENDER_TIMEOUT_MS, async () => {
                    await this.command(session, "Page.startScreencast", {
                        format: "jpeg", quality: 10, maxWidth: 16, maxHeight: 16, everyNthFrame: 1,
                    }, generation);
                    await frame;
                });
                if (!this.authorized(generation)) throw new Error("CDP authorization revoked");
                const output = await bounded("Native screenshot capture", CAPTURE_TIMEOUT_MS,
                    () => this.command(session, "Page.captureScreenshot", captureParams, generation));
                if (typeof output.data !== "string" || output.data.length > MAX_SCREENSHOT_BASE64) {
                    throw new Error("Oversized or invalid screenshot output; maximum 500000 decoded bytes");
                }
                await bounded("Screenshot render cleanup", RENDER_TIMEOUT_MS, async () => {
                    await this.command(session, "Page.stopScreencast", {}, generation);
                    await capture.ack;
                    const metrics = await this.command(session, "Page.getLayoutMetrics", {}, generation);
                    if ((metrics.cssVisualViewport?.zoom ?? 1) !== pageZoom) {
                        throw new Error("Page zoom changed during screenshot capture; output not returned");
                    }
                });
                return output;
            } catch (error) {
                error.retireBridge = true;
                throw error;
            } finally { if (this.capture === capture) this.capture = null; }
        }

        captureEvent(source, method, params) {
            if (source.sessionId || !["Page.screencastFrame", "Page.screencastVisibilityChanged"].includes(method)) return false;
            const capture = this.capture;
            if (method !== "Page.screencastFrame" || !capture || !this.authorized(capture.generation)) return true;
            if (!Number.isSafeInteger(params.sessionId) || params.sessionId <= 0) {
                capture.reject(new Error("Invalid private screenshot frame identity"));
                return true;
            }
            if (capture.frameId === null) {
                if (params.sessionId <= this.lastCaptureFrameId) return true;
                capture.frameId = params.sessionId;
                this.lastCaptureFrameId = params.sessionId;
            } else if (params.sessionId !== capture.frameId) {
                if (params.sessionId > capture.frameId) capture.reject(new Error("Conflicting screenshot frame identity"));
                return true;
            }
            if (capture.pendingFrames.length >= 2) {
                capture.reject(new Error("Screenshot frame acknowledgement backpressure"));
                return true;
            }
            capture.pendingFrames.push(params.sessionId);
            if (capture.ackDraining) return true;
            capture.ackDraining = true;
            capture.ack = (async () => {
                try {
                    while (capture.pendingFrames.length && this.capture === capture && this.authorized(capture.generation)) {
                        const sessionId = capture.pendingFrames.shift();
                        await this.command(capture.session, "Page.screencastFrameAck", {sessionId}, capture.generation);
                        if (this.capture === capture && this.authorized(capture.generation)) capture.resolve();
                    }
                } catch (error) { capture.reject(error); }
                finally { capture.ackDraining = false; }
            })();
            return true;
        }

        async handle(message) {
            const generation = this.generation;
            if (!this.authorized(generation)) return;
            const response = {id: message?.id};
            if (message?.sessionId !== undefined) response.sessionId = message.sessionId;
            try {
                if (!message || !Number.isSafeInteger(message.id) ||
                    typeof message.method !== "string" ||
                    (message.sessionId !== undefined && typeof message.sessionId !== "string") ||
                    (message.params !== undefined && (!message.params ||
                    typeof message.params !== "object" || Array.isArray(message.params)))) {
                    throw new Error("Malformed CDP request");
                }
                const {method, sessionId} = message;
                const params = copy(message.params || {});
                const session = sessionId === undefined ? null : this.session(sessionId);
                let result;
                if (!session) {
                    if (method === "Browser.getVersion") {
                        if (Object.keys(params).length) throw new Error("Unexpected version parameters");
                        // chrome.debugger has no Browser domain; Playwright's extension relay answers locally.
                        result = {protocolVersion: "1.3", product: "Chrome/Extension-Bridge",
                            userAgent: root.navigator?.userAgent || "CDP-Bridge-Server/1.0.0"};
                    } else if (method === "Target.setAutoAttach") {
                        this.validateAutoAttach(params);
                        await this.attachRoot(generation);
                        result = {};
                    } else if (method === "Target.getTargetInfo" || method === "Target.getTargets") {
                        if (Object.keys(params).some(key => key !== "targetId") ||
                            (params.targetId !== undefined &&
                            params.targetId !== this.targetInfo?.targetId)) {
                            throw new Error("Foreign target lookup denied");
                        }
                        const info = await this.exactTarget(generation);
                        result = method === "Target.getTargets" ? {targetInfos: [copy(info)]} :
                            {targetInfo: copy(info)};
                    } else {
                        throw new Error(`Root CDP command denied: ${method}`);
                    }
                } else if (method === "Target.setAutoAttach") {
                    this.validateAutoAttach(params);
                    result = await this.command(session, method,
                        {...params, filter: copy(FRAME_FILTER)}, generation);
                } else {
                    if (!PAGE_COMMANDS.has(method)) throw new Error(`Page CDP command denied: ${method}`);
                    if (method === "Network.getResponseBody") {
                        const size = this.bodies.get(`${session.id}:${params.requestId}`);
                        if (!size?.finished || !size.observed || size.bytes > MAX_BODY) {
                            throw new Error(size?.bytes > MAX_BODY ? "Body oversized" :
                                "Body size unavailable or unfinished");
                        }
                    }
                    if (method === "Page.captureScreenshot") {
                        result = await this.captureScreenshot(session, params, generation);
                    } else result = await this.command(session, method, params, generation);
                }
                if (!this.authorized(generation)) return;
                if (session && this.sessions.get(session.id) !== session) {
                    throw new Error("CDP session detached before response");
                }
                this.emit({...response, result}, generation);
            } catch (error) {
                if (!this.authorized(generation)) return;
                this.emit({...response, error: {code: -32000, message: String(error.message || error)}},
                    generation);
                if (error.retireBridge) { this.close(); this.report(error); }
            }
        }

        event(source, method, params) {
            if (source.tabId !== this.tabId || !this.authorized()) return;
            if (this.captureEvent(source, method, params)) return;
            const parentId = source.sessionId || this.rootSessionId;
            const parent = this.sessions.get(parentId);
            if (!parent) {
                // Exact root events during our lookup and focus preparation cannot precede attachment.
                if (!source.sessionId && (this.bootstrap || this.rootAttachment) &&
                    ((method === "Target.attachedToTarget" && params.targetInfo?.type === "iframe") ||
                    (method === "Target.detachedFromTarget" && this.buffer.some(event =>
                        event[1] === "Target.attachedToTarget" &&
                        event[2].sessionId === params.sessionId)) ||
                    (EVENT_DOMAINS.has(method.split(".")[0]) && method !== "Page.windowOpen"))) {
                    if (this.buffer.length >= 256) {
                        this.close();
                        throw new Error("CDP bootstrap event buffer exceeded");
                    }
                    this.buffer.push([copy(source), method, params]);
                }
                return;
            }
            if (method === "Target.attachedToTarget") {
                if (params.targetInfo?.type !== "iframe" ||
                    typeof params.targetInfo.targetId !== "string" ||
                    !params.targetInfo.targetId || typeof params.sessionId !== "string" ||
                    !params.sessionId || this.sessions.has(params.sessionId) ||
                    this.retiredSessions.has(params.sessionId) ||
                    [...this.sessions.values()].some(session =>
                        session.targetId === params.targetInfo.targetId)) return;
                this.sessions.set(params.sessionId, {id: params.sessionId, parent: parentId,
                    targetId: params.targetInfo.targetId});
            } else if (method === "Target.detachedFromTarget") {
                const child = this.sessions.get(params.sessionId);
                if (!child || child.parent !== parentId ||
                    (params.targetId !== undefined && params.targetId !== child.targetId)) return;
                this.removeSession(child.id);
            } else if (!EVENT_DOMAINS.has(method.split(".")[0]) || method === "Page.windowOpen") {
                return;
            }
            const key = `${parentId}:${params.requestId}`;
            if (method === "Network.requestWillBeSent") {
                this.bodies.set(key, {bytes: 0, observed: false, finished: false});
                if (this.bodies.size > MAX_CAPTURE) this.bodies.delete(this.bodies.keys().next().value);
            }
            const size = this.bodies.get(key);
            if (size && method === "Network.dataReceived") {
                const bytes = params.dataLength;
                size.observed = true;
                size.bytes = Number.isSafeInteger(bytes) && bytes >= 0 ?
                    Math.min(MAX_BODY + 1, size.bytes + bytes) : MAX_BODY + 1;
            }
            if (size && method === "Network.loadingFinished") size.finished = true;
            if (size && method === "Network.loadingFailed") this.bodies.delete(key);
            this.emit({sessionId: parentId, method, params});
        }

        removeSession(id) {
            for (const child of this.sessions.values()) {
                if (child.parent === id) this.removeSession(child.id);
            }
            this.sessions.delete(id);
            for (const key of this.bodies.keys()) {
                if (key.startsWith(`${id}:`)) this.bodies.delete(key);
            }
            this.retiredSessions.add(id);
        }

        close() {
            if (this.closed) return;
            this.closed = true;
            this.generation++;
            this.capture?.reject(new Error("Screenshot authorization retired"));
            this.capture = null;
            this.sessions.clear();
            this.bodies.clear();
            this.retiredSessions.clear();
            this.targetInfo = null;
            this.bootstrap = null;
            this.rootAttachment = null;
            this.buffer = [];
            this.chrome.debugger.onEvent.removeListener(this.eventListener);
            this.chrome.debugger.onDetach.removeListener(this.detachListener);
        }
    }

    root.AuthorizedCdpBridge = AuthorizedCdpBridge;
    if (typeof module !== "undefined" && module.exports) module.exports = {AuthorizedCdpBridge};
})(globalThis);
