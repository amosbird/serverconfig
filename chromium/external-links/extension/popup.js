const toggle = document.getElementById("toggle");
const state = document.getElementById("state");
const error = document.getElementById("error");
const list = document.getElementById("workbenches");
let automationOn = true;
let generation = 0;
async function control(action) {
    const current = ++generation;
    toggle.disabled = action === "stop";
    if (action === "start") automationOn = true;
    try {
        const response = await chrome.runtime.sendMessage({control: action});
        if (current !== generation) return;
        if (!response?.ok) throw new Error(response?.error || "Browser integration unavailable");
        automationOn = response.automation_on;
        toggle.textContent = automationOn ? "Stop automation" : "Start automation";
        state.textContent = automationOn ? "Automation on · workbenches prepare automatically" :
            "Automation off · pages remain open";
        list.replaceChildren(...response.remotes.map(remote => {
            const item = document.createElement("li");
            const status = remote.creation_unknown ?
                "Error: tab creation unknown. Inspect the partial tab; no automatic retry." :
                remote.cleanup_pending ? "Error: debugger cleanup pending" :
                !automationOn ? "Off" : remote.error ? `Error: ${remote.error}` : remote.agreed ? "Ready" :
                remote.connected ? "Unavailable / preparing" : "Disconnected";
            item.textContent = `${remote.display}: ${status}`;
            return item;
        }));
        error.textContent = response.error || "";
    } catch (failure) {
        if (current === generation) error.textContent = failure.message;
    } finally {
        if (current === generation) toggle.disabled = false;
    }
}
toggle.addEventListener("click", () => control(automationOn ? "stop" : "start"));
control("status");
