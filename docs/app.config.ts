// https://github.com/nuxt-themes/docus/blob/main/nuxt.schema.ts
export default defineAppConfig({
  docus: {
    title: 'Codchi',
    description: 'Reproducible and Reliable Development Environments',
    socials: {
      github: 'aformatik/codchi',
    },
    github: {
      dir: 'docs',
      branch: 'master',
      repo: 'codchi',
      owner: 'aformatik',
      edit: true
    },
    aside: {
      level: 0,
      collapsed: false,
      exclude: []
    },
    main: {
      padded: true,
      fluid: true
    },
    header: {
      logo: true,
      showLinkIcon: true,
      exclude: [],
      fluid: true
    }
  }
})
