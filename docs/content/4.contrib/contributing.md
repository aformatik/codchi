# Contribution Guidelines

## Workflow

### New features (todo)

- [ ] PR per feature (try to limit changes in a single PR)
- [ ] Code Review
- [ ] Build & Test in CI

### New release

- push tag to automatically create a pre-release
- automatic tests run in CI (todo: linux tests)
- do manual tests (todo: test checklist)
- update changelog & release description (only full release)
    => post changes to discourse

## Commits

Use [conventional commits](https://www.conventionalcommits.org/en/v1.0.0/#summary):

- feat: for new features
- fix: for bug fixes
- docs: for documentation updates
- refactor: for code refactoring
- test: for test-related changes
- chore: for maintenance tasks

## Changelog

- List all commits since the last relase grouped by the commit type (Feature, Bug Fix...)

## Release

Include the changelog since the last **full** release
