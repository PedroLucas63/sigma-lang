import fs from 'node:fs'
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import { defineConfig } from 'vitepress'

const __dirname = path.dirname(fileURLToPath(import.meta.url))
const webRoot = path.resolve(__dirname, '..')
const repoRoot = path.resolve(webRoot, '..')
const sigmaGrammar = JSON.parse(
  fs.readFileSync(path.join(repoRoot, 'editors/vscode/syntaxes/sigma.tmLanguage.json'), 'utf8')
)

export default defineConfig({
  title: 'Sigma',
  description: 'Linguagem de programação estaticamente tipada de propósito geral e computação científica com desempenho nativo C99.',
  lang: 'pt-BR',
  srcDir: '../docs',
  outDir: './.vitepress/dist',
  base: process.env.VITEPRESS_BASE || '/sigma-lang/',
  ignoreDeadLinks: true,

  markdown: {
    languages: [
      {
        ...sigmaGrammar,
        name: 'sigma',
        aliases: ['sgm']
      }
    ]
  },

  vite: {
    resolve: {
      alias: {
        vue: path.join(webRoot, 'node_modules/vue'),
        'vue/server-renderer': path.join(webRoot, 'node_modules/vue/server-renderer')
      }
    }
  },

  themeConfig: {
    search: {
      provider: 'local'
    },

    nav: [
      { text: 'Início', link: '/' },
      { text: 'Começar', link: '/guide/00-getting-started' },
      { text: 'Guia', link: '/guide/01-variables-and-types' },
      { text: 'Especificação', link: '/spec/grammar-and-types' },
      { text: 'CLI', link: '/tools/cli-reference' },
      { text: 'Playground', link: '/playground.html', target: '_self' }
    ],

    sidebar: [
      {
        text: 'Começando',
        items: [
          { text: 'Instalação e Primeiros Passos', link: '/guide/00-getting-started' },
          { text: 'Visão Geral da Documentação', link: '/README' }
        ]
      },
      {
        text: 'Guia da Linguagem',
        items: [
          { text: '01. Variáveis, Tipos, Decimal e E/S', link: '/guide/01-variables-and-types' },
          { text: '02. Operadores e Expressões', link: '/guide/02-operators-and-expressions' },
          { text: '03. Condicionais, Laços e Blocos', link: '/guide/03-control-flow-and-blocks' },
          { text: '04. Testes e Projetos (sigma.toml)', link: '/guide/04-testing-and-project-management' },
          { text: '05. Tuplas, Arrays e Referências', link: '/guide/05-tuples-and-arrays' },
          { text: '06. Funções, Act, Closures e ref/out', link: '/guide/06-functions-and-actions' },
          { text: '07. Class, Struct, Record e new', link: '/guide/07-user-defined-types' },
          { text: '08. Ponto de Entrada main e Scripts', link: '/guide/08-entry-points-and-scripts' }
        ]
      },
      {
        text: 'Referência & Ferramentas',
        items: [
          { text: 'Gramática, Tipos e Diagnósticos', link: '/spec/grammar-and-types' },
          { text: 'Referência da CLI (sigma)', link: '/tools/cli-reference' }
        ]
      }
    ],

    socialLinks: [
      { icon: 'github', link: 'https://github.com/PedroLucas63/sigma-lang' }
    ],

    footer: {
      message: 'Linguagem Sigma v0.2.3 — Interpretador VM + Compilador Nativo C99',
      copyright: 'MIT License'
    }
  }
})
