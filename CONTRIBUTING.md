# Contributing to inet.data

Thanks for your interest in improving `inet.data`. Bug reports, fixes, and
focused feature contributions are all welcome.

## Before you start

- For a change that is more than a trivial fix, **open an issue first**. This
  lets us agree on the approach before you spend time on it.
- Read the existing issues and pull requests to prevent duplicate work.

## Development

This is a Clojure library. You need a JDK and [Leiningen](https://leiningen.org/).
Projects that moved to `deps.edn` use the Clojure CLI instead: see the README.

```bash
lein test     # run the test suite
lein check    # AOT-compile; must be free of reflection warnings
```

A change is mergeable when it obeys these rules:

- **Tests first.** Add or update tests for the behavior you change. For a bug
  fix, add a regression test that fails before your fix and passes after it.
- **Green build.** `lein test` passes and `lein check` reports **zero**
  reflection warnings.
- **No scope creep.** Keep each pull request to one logical change.

## Commits and pull requests

- Follow [Conventional Commits](https://www.conventionalcommits.org/)
  (`feat:`, `fix:`, `docs:`, `refactor:`, `test:`, `chore:` …).
- Keep the subject in the imperative mood and under ~72 characters.
- Update `CHANGELOG.md` when your change is user-visible.
- Rebase on the latest `main` before opening the pull request.

## License

By contributing, you agree that your contributions will be licensed under the
same license as this project (see `LICENSE` / the README).
