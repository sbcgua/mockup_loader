# Website

This website is built using [Docusaurus](https://docusaurus.io/), a modern static website generator.

## Installation

```sh
npm install
```

## Local Development

```sh
npm start
```

This command starts a local development server and opens up a browser window. Most changes are reflected live without having to restart the server.

## Build

```sh
npm build
```

This command generates static content into the `build` directory and can be served using any static contents hosting service.

## Deployment

Using SSH:

```sh
USE_SSH=true npm deploy
```

Not using SSH:

```sh
GIT_USER=<Your GitHub username> npm deploy
```

If you are using GitHub pages for hosting, this command is a convenient way to build the website and push to the `gh-pages` branch.

## Notes

- `npm install --save @docusaurus/plugin-client-redirects` to support redirect from `/docs/` to `/docs/intro`
- redirect gh-actions to master
- logo works in wip
- prism `additionalLanguages: ['ABAP']`
- add google analytics
- if blog link is above `truncate` is must be complete (start with `/`), otherwise will be broken on tags page
- change Repo/Settings/Pages/Source to Github actions (otherwise does not deploy from GA) - after that the pages are based on the GA artifact not the `gh-pages` branch! (TODO: maybe check how to deploy to a branch ... but maybe not)
- Algolia alternatives: https://typesense.org/docs/overview/comparison-with-alternatives.html#typesense-vs-algolia
