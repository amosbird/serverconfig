const toggle = document.getElementById("toggle");
const state = document.getElementById("state");
const error = document.getElementById("error");
const list = document.getElementById("workbenches");
let automationOn = true;
let stopPending = false;
let generation = 0;
async function control(action, details = {}) {
    const current = ++generation;
    toggle.disabled = action === "stop";
    if (action === "start") automationOn = true;
    try {
        const response = await chrome.runtime.sendMessage({control: action, ...details});
        if (current !== generation) return;
        if (typeof response?.automation_on !== "boolean") throw new Error(response?.error || "Browser integration unavailable");
        automationOn = response.automation_on;
        stopPending = response.stop_pending === true;
        toggle.textContent = stopPending ? "Retry Stop" : automationOn ? "Stop automation" : "Start automation";
        const executorPhase = response.browser_executor?.phase;
        state.textContent = stopPending ? "Automation off locally · Stop not saved · retry before restarting" :
            response.preference_pending ? "Automation preference pending · browser work blocked" :
            !automationOn ? "Automation off · pages remain open" :
            executorPhase === "ready" ? "Automation on · work tabs open on first navigation" :
            executorPhase === "pending" ? "Automation preference on · browser executor preparing" :
            executorPhase === "secondary" ? "Automation preference on · use the configured primary browser" :
            "Automation preference on · browser executor unavailable";
        list.replaceChildren(...response.remotes.map(remote => {
            const item = document.createElement("li");
            const status = remote.creation_unknown ?
                "Error: tab creation unknown. Inspect the partial tab; no automatic retry." :
                remote.cleanup_pending ? "Error: debugger cleanup pending" :
                !automationOn ? "Off" : remote.error ? `Error: ${remote.error}` : remote.agreed ? "Ready" :
                remote.connected ? "Unavailable / preparing" : "Disconnected";
            item.textContent = `${remote.display}: ${status}`;
            if (remote.creation_unknown) {
                const recover = document.createElement("button");
                recover.textContent = "Abandon unknown preparation";
                recover.addEventListener("click", () => {
                    if (confirm("Leave all partial pages open and abandon this unknown preparation? A separate fresh preparation is allowed only while automation is on.")) {
                        control("recover", {remote_key: remote.remote_key, confirmed: true});
                    }
                });
                item.append(recover);
            }
            return item;
        }));
        error.textContent = response.error || response.browser_executor?.error || "";
    } catch (failure) {
        if (current === generation) error.textContent = failure.message;
    } finally {
        if (current === generation) toggle.disabled = false;
    }
}
toggle.addEventListener("click", () => control(automationOn || stopPending ? "stop" : "start"));
control("status");
