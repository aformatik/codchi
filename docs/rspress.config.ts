import { defineConfig } from 'rspress/config';
import { RspressPlugin } from '@rspress/shared';
import path from 'path';

export function global(): RspressPlugin {
  return {
    // plugin name
    name: 'global',
    globalStyles: path.join(__dirname, 'src/global.css'),
  };
}

export default defineConfig({
  root: 'src',
  plugins: [global()],

  title: "codchi",
  description: "CODe maCHInes - Define your Development Environment Once - Run Anywhere on Windows and Linux",

  icon: '/favicon.ico',
  logo: '/favicon.ico',
  logoText: 'codchi',

  themeConfig: {
    socialLinks: [
      { icon: 'github', mode: 'link', content: 'https://github.com/aformatik/codchi' },
    ],
  },

});
