import { defineConfig } from 'rspress/config';
import { RspressPlugin } from '@rspress/shared';
import path from 'path';
import { pluginShiki } from '@rspress/plugin-shiki';

export function global(): RspressPlugin {
    return {
        // plugin name
        name: 'global',
        globalStyles: path.join(__dirname, 'src/global.css'),
    };
}

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
            "/docs/": [
                {
                    text: "Getting Started",
                    items: [
                        "/docs/start/intro",
                        "/docs/start/installation",
                        "/docs/start/first-machine",
                        "/docs/start/usage",
                        "/docs/start/config",
                        "/docs/start/troubleshooting"
                    ]
                },
                // {
                //     text: "Module Configuration",
                //     items: ["architecture"]
                // },
                {
                    text: "Module Configuration",
                    items: [
                        "/docs/config/overview",
                        "/docs/config/start",
                        "/docs/config/secrets"
                    ],
                },
                {
                    link: "/docs/options",
                    text: "NixOS Options Reference"
                }
            ]
        },
        footer: {
            // copyright: "Contributors",
            message:
                '&copy; Codchi contributors <br> <a href="https://aformatik.de/rechtliches/">Impressum</a>',
        },
    },

});
