# Contributing to Morpheus GraphQL

Thank you for your interest in contributing!

## Pull Request Labels

We use **semantic labels** for pull requests, powered by [relasy](https://github.com/nalchevanidze/relasy). Please make sure to select the appropriate semantic label (such as `feat`, `fix`, `chore`, etc.) when opening or updating a PR. This helps automate release notes and maintain a clear project history.

Relasy also provides labels for the **scope** of your update, using package emojis. For example:

    - 📦 server
    - 📦 codegen

Using a scope label is recommended for clarity, but not required.

If you are unsure which label or scope to use, see the relasy documentation or ask in an issue or our Slack channel.

## How to Contribute

- Please open issues for bugs, feature requests, or questions.
- Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

## Development Workflow

This project uses the [hwm](https://github.com/nalchevanidze/hwm) monorepo tool to automate builds, formatting, linting, and code generation for Haskell projects.

### Updating Dependencies

To update dependencies, use:

```
hwm outdated --fix
```

This will update dependencies across the monorepo. After updating dependencies or making changes that affect the build, always run:

```
hwm lint
hwm format
hwm status
hwm sync
```

This will check for linting issues, apply formatting, and verify that all generated files are up to date.

## Code Style

- Please follow the formatting and linting rules enforced by hwm.
- Run `hwm lint` and `hwm format` before submitting a pull request.

## Questions?

Feel free to open an issue or join our [Slack channel](https://morpheus-graphql.slack.com) for help.

Happy hacking!
