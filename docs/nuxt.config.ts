export default defineNuxtConfig({
    // https://github.com/nuxt-themes/docus
    extends: ['@nuxt-themes/docus'],
    devtools: { enabled: true },
    content: {
        highlight: {
            preload: ["nix", "toml", "ps1"],
        },
    },

    modules: [
        // Remove it if you don't use Plausible analytics
        // https://github.com/nuxt-modules/plausible
        // '@nuxtjs/plausible'
    ],
    // ssr: true,
    compatibilityDate: '2024-10-24'
})
