const ROUTES = {
    groups: {
        ai: {
            sites: ["chatgpt.com", "claude.ai", "gemini.google.com"],
            suffixes: [],
        },
        chat: {
            sites: ["discord.com", "web.whatsapp.com", "app.slack.com", "slack.com"],
            suffixes: [".slack.com"],
        },
    },
    authSites: [
        "accounts.google.com",
        "appleid.apple.com",
        "github.com",
        "login.microsoftonline.com",
    ],
};

if (typeof module !== "undefined") module.exports = {ROUTES};
