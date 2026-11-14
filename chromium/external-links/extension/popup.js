const select = document.getElementById("remote");
const state = document.getElementById("state");
const error = document.getElementById("error");
let remotes = [];
let controlGeneration = 0;
function render() {
    const remote = remotes.find(item => item.remote_key === select.value);
    state.textContent = remote ? remote.creation_unknown ?
        "Creation uncertain. Inspect the partial tab. Disconnect leaves it ordinary; Enable then creates a fresh tab." :
        `${remote.state.phase}${remote.agreed ? " · workbench agreed" : " · not authorized"}` :
        "No connected authenticated workbench. Start browser integration in Abush, then Refresh.";
}
async function control(action) {
    const generation = ++controlGeneration;
    const buttons = [...document.querySelectorAll("button")];
    buttons.filter(button => !["pause", "takeover", "disconnect"].includes(button.dataset.action))
        .forEach(button => { button.disabled = true; });
    error.textContent = "";
    try {
        const response = await chrome.runtime.sendMessage({control: action, remote_key: select.value});
        if (generation !== controlGeneration) return;
        if (!response?.ok) throw new Error(response?.error || "Browser integration did not respond");
        remotes = response.remotes;
        const previous = select.value;
        select.replaceChildren(...remotes.map(remote => {
            const option = document.createElement("option");
            option.value = remote.remote_key;
            option.textContent = `${remote.display} (${remote.principal_id || "disconnected"})`;
            return option;
        }));
        if (remotes.some(remote => remote.remote_key === previous)) select.value = previous;
        error.textContent = response.error || "";
        render();
    } catch (failure) {
        if (generation === controlGeneration) error.textContent = failure.message;
    } finally {
        if (generation === controlGeneration) buttons.forEach(button => { button.disabled = false; });
    }
}
select.addEventListener("change", render);
document.querySelectorAll("button").forEach(button =>
    button.addEventListener("click", () => control(button.dataset.action)));
control("refresh");
