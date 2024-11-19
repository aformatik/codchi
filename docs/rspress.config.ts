import { defineConfig } from 'rspress/config';
import { RspressPlugin } from '@rspress/shared';
import path from 'path';
import fs from 'fs';
import { pluginShiki } from '@rspress/plugin-shiki';

export function global(): RspressPlugin {
    return {
        // plugin name
        name: 'global',
        globalStyles: path.join(__dirname, 'src/global.css'),
    };
}

const sidebar = [];
sidebar.push({
    text: "Getting Started",
    // collapsible: false,
    items: [
        "/docs/start/intro",
        "/docs/start/installation",
        "/docs/start/first-machine",
        "/docs/start/config",
        "/docs/start/troubleshooting"
    ]
})
if (fs.existsSync(path.join(__dirname, 'src/docs/usage'))) {
    sidebar.push({
        text: "Command Reference",
        items: [
            "/docs/usage/codchi",
            "/docs/usage/status",
            "/docs/usage/init",
            "/docs/usage/clone",
            {
                text: "Module",
                collapsible: false,
                items: [
                    "/docs/usage/module/module",
                    "/docs/usage/module/list",
                    "/docs/usage/module/add",
                    "/docs/usage/module/set",
                    "/docs/usage/module/delete",
                ],
            },
            "/docs/usage/rebuild",
            "/docs/usage/exec",
            "/docs/usage/delete",
            "/docs/usage/gc",
            "/docs/usage/completion",
        ],
    })
}
// {
//     text: "Module Configuration",
//     items: ["architecture"]
// }
sidebar.push({
    text: "Module Configuration",
    items: [
        "/docs/config/overview",
        "/docs/config/start",
        "/docs/config/secrets",
        "/docs/config/misc",
        "/docs/config/devenv",
        "/docs/config/editor",
        {
            text: "Environments",
            collapsible: false,
            items: [
                "/docs/config/environments/nix",
                "/docs/config/environments/jvm",
                "/docs/config/environments/javascript",
                "/docs/config/environments/python",
                "/docs/config/environments/gpu",
                // "/docs/config/examples/c",
                // "/docs/config/examples/rust",
                // "/docs/config/examples/go",
                // "/docs/config/examples/ai",
            ],
        },
    ],
})
sidebar.push({
    link: "/docs/options",
    text: "NixOS Options Reference"
})



export default defineConfig({
    // base: '/new/',
    root: 'src',
    plugins: [
        global(),
        pluginShiki({
            langs: ["nix"]
        })
    ],

    title: "Codchi",
    description: "CODe maCHInes - Define your Development Environment Once - Run Anywhere on Windows and Linux",

    icon: '/favicon.ico',
    logo: '/favicon.ico',
    logoText: 'Codchi',

    themeConfig: {
        socialLinks: [
            { icon: 'github', mode: 'link', content: 'https://github.com/aformatik/codchi' },
        ],
        nav: [
            {
                text: 'Docs',
                link: '/docs/start/intro',
                position: 'right',
            },
            {
                text: 'Download',
                items: [
                    {
                        text: 'Windows (msix)',
                        link: 'https://github.com/aformatik/codchi/releases/latest/download/codchi.msix',
                    },
                ],
                position: 'right',
            },
        ],
        sidebar: {
            "/docs/": sidebar
        },
        footer: {
            // copyright: "Contributors",
            message:
                '&copy; Codchi contributors <br> <a href="https://aformatik.de/rechtliches/">Impressum</a>',
        },
    },

});
