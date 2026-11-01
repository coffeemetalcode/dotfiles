# Copilot instructions for this Doom Emacs config

This repository contains a local Doom Emacs configuration managed in `$HOME/.config/doom`.

## Project purpose

This config is for personal Emacs/Doom setup and customization. It is not a general application project and does not have a normal build/test pipeline.

## Required research policy

Before making changes involving Doom Emacs, its modules, package configuration, or any plugin/extension, use Context7 MCP to fetch the relevant official documentation first.

- Prefer official documentation over third-party repositories or blog posts whenever both are available.
- For Doom Emacs topics, use the official Doom Emacs docs as the primary source.
- For package/plugin behavior and APIs, prefer the package's official docs or authoritative project docs via Context7.
- When the package version is relevant, consult the current version in use before choosing documentation or source references.
- Use the current package version in this local configuration as the basis for version-appropriate guidance whenever possible.

## Key files

- `init.el`: Doom module selection and load order.
- `config.el`: runtime configuration, package setup, UI/theme settings, and custom behavior.
- `packages.el`: extra packages installed via Doom's package manager.
- `custom.el`: generated user customizations; keep machine-local changes here if necessary.

## Working conventions

- Keep changes consistent with Doom Emacs conventions and Lisp style.
- Prefer `after!` and `use-package!` patterns in `config.el` when customizing packages.
- If you add or remove packages, also update `packages.el` and run `doom sync` after changes.
- If you change enabled Doom modules in `init.el`, run `doom sync` and restart Emacs to reload settings.
- Preserve the existing personal/local nature of the config; avoid broad refactors unrelated to the current task.
- Use two-space indentation in config snippets where the project already uses it for web/markup styles, and keep Lisp forms aligned with Doom conventions.
- Preserve comments and local explanations already in the config unless a change clearly requires updating them.

## AI assistant guidance

- Treat this as a personal dotfiles repository, not a library or service project.
- Prefer surgical edits that match the existing configuration style and naming patterns.
- Avoid introducing non-essential packages or heavy global configuration changes.
- When editing `config.el`, keep custom behavior local and predictable.
- When touching the Copilot integration, respect the repo's already-configured setup in `init.el` and `config.el`.
- If a task touches Emacs or package behavior, ground the answer in official docs and current version context instead of general memory alone.

## Validation

- There is no conventional automated test suite for this repo.
- Validate changes by checking for Emacs Lisp syntax correctness and by reloading the config in Doom when relevant.
- For package or module changes, prefer `doom sync` followed by a config reload.

## Notes

This config currently includes Doom modules for completion, UI, editor, tools, languages, and Copilot support. Keep the setup minimal and stable while making targeted improvements.
