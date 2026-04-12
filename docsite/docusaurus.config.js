// @ts-check
// See: https://docusaurus.io/docs/api/docusaurus-config

import {themes as prismThemes} from 'prism-react-renderer';

// This runs in Node.js - Don't use client-side code here (browser APIs, JSX...)

const githubUrl = 'https://github.com';
const organizationName = 'sbcgua';
const projectName = 'mockup_loader';
const repoUrl = `${githubUrl}/${organizationName}/${projectName}`

const linkedinHref = (/** @type {string} */ name) => `<a href="${name}" target="_blank" rel="noopener noreferrer" title="LinkedIn" style="vertical-align: middle; text-decoration: none;">
    <svg xmlns="http://www.w3.org/2000/svg" width="1em" height="1em" preserveAspectRatio="xMidYMid" viewBox="0 0 256 256" class="linkedinSvg_FCgI" style="--dark: #0a66c2; --light: #ffffffe6;">
    <path d="M218.123 218.127h-37.931v-59.403c0-14.165-.253-32.4-19.728-32.4-19.756 0-22.779 15.434-22.779 31.369v60.43h-37.93V95.967h36.413v16.694h.51a39.907
      39.907 0 0 1 35.928-19.733c38.445 0 45.533 25.288 45.533 58.186l-.016 67.013ZM56.955 79.27c-12.157.002-22.014-9.852-22.016-22.009-.002-12.157 9.851-22.014
      22.008-22.016 12.157-.003 22.014 9.851 22.016 22.008A22.013 22.013 0 0 1 56.955 79.27m18.966 138.858H37.95V95.967h37.97v122.16ZM237.033.018H18.89C8.58-.098.125
      8.161-.001 18.471v219.053c.122 10.315 8.576 18.582 18.89 18.474h218.144c10.336.128 18.823-8.139 18.966-18.474V18.454c-.147-10.33-8.635-18.588-18.966-18.453">
    </path>
    </svg>
    </a>`; // reconsider this hardcoded HTML, see also @docusaurus\theme-classic\lib\theme\Icon\Socials\LinkedIn\index.js
const myLinkedin = linkedinHref('https://www.linkedin.com/in/atsybulsky/');

/** @type {import('@docusaurus/types').Config} */
const config = {

  future: {
    v4: true,
    experimental_faster: true,
  },

  title:   'Mockup Loader',
  tagline: 'Mockup loader - the right tool to simplify data preparation for SAP ABAP unit tests',
  favicon: 'img/favicon.ico',

  // Site URLs
  url: githubUrl,               // Set the production url of the site here
  baseUrl: `/${projectName}/`,  // Set the /<baseUrl>/ pathname under which your site is served, @github usually '/<projectName>/'

  // GitHub pages deployment config
  organizationName,   // GitHub org/user name
  projectName,        // Repo name

  onBrokenLinks: 'throw',

  markdown: {
    hooks: {
      onBrokenMarkdownLinks: 'warn',
    },
  },

  i18n: {
    defaultLocale: 'en',
    locales: ['en'],
  },

  presets: [
    [
      'classic',
      /** @type {import('@docusaurus/preset-classic').Options} */
      {
        docs: {
          sidebarPath: './sidebars.js',
        },
        blog: {
          showReadingTime: true,
          feedOptions: {
            type: ['rss', 'atom'],
            xslt: true,
          },
          // Useful options to enforce blogging best practices
          onInlineTags: 'warn',
          onInlineAuthors: 'warn',
          onUntruncatedBlogPosts: 'warn',
        },
        theme: {
          customCss: './src/css/custom.css',
        },
        gtag: {
          trackingID: 'G-0QEBYGR127',
          anonymizeIP: true,
        },
      },
    ],
  ],

  themeConfig:
    /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    {
      image: 'img/logo-128.png',
      navbar: {
        title: 'Mockup loader',
        logo: {
          alt: 'Mockup loader Logo',
          src: 'img/logo.svg',
        },
        items: [
          {
            type: 'docSidebar',
            sidebarId: 'tutorialSidebar',
            position: 'left',
            label: 'Documentation',
          },
          {
            to: '/blog',
            label: 'Blog',
            position: 'left'
          },
          {
            href: repoUrl,
            label: 'GitHub',
            position: 'right',
          },
        ],
      },
      footer: {
        style: 'dark',
        links: [
          {
            label: 'Documentation',
            to: '/docs/intro',
          },
          {
            label: 'Blog',
            to: '/blog',
          },
          {
            label: 'GitHub',
            href: repoUrl,
          },
        ],
        copyright: `Copyright © ${new Date().getFullYear()} Alexander Tsybulsky ${myLinkedin} (aka sbcgua), Built with Docusaurus.`,
      },
      prism: {
        theme: prismThemes.github,
        darkTheme: prismThemes.dracula,
        additionalLanguages: ['abap'],
      },
      algolia: {
        appId: 'JYRGWOZND5',
        apiKey: '7f7a2954f42dc2446a9c7bf3c7f305af',
        indexName: 'sbcgua-mockup-loader',
        contextualSearch: false, // No versions and languages so far

        // Optional: Specify domains where the navigation should occur through window.location instead on history.push. Useful when our Algolia config crawls multiple documentation sites and we want to navigate with window.location.href to them.
        // externalUrlRegex: 'external\\.com|domain\\.com',
        // Optional: Replace parts of the item URLs from Algolia. Useful when using the same search index for multiple deployments using a different baseUrl. You can use regexp or string in the `from` param. For example: localhost:3000 vs myCompany.com/docs
        // replaceSearchResultPathname: {
        //   from: '/docs/', // or as RegExp: /\/docs\//
        //   to: '/',
        // },

        // Optional: Algolia search parameters
        // searchParameters: {},

        // Optional: path for search page that enabled by default (`false` to disable it)
        searchPagePath: 'search',
        // Optional: whether the insights feature is enabled or not on Docsearch (`false` by default)
        insights: false,
      },
    },

    plugins: [
      [
        '@docusaurus/plugin-client-redirects',
        {
          redirects: [
            {
              from: ['/docs'],
              to: '/docs/intro',
            },
          ],
        },
      ],
    ],
};

export default config;
