# How to contribute to the Swirl project?

First of all, thank you very much for passing by and for your interest in
contributing to the [Swirl project](http://www.swirl-project.org/). We hope
you will find all needed information in this document. If you have any
questions, please
[get in touch with us](http://www.swirl-project.org/#contributing).

## People

- Dave Cottlehuber [@dch](http://twitter.com/dch__)
- Andy Wenk [@awenkhh](http://twitter.com/awenkhh)
- Vansheep Singh [@kansi](https://github.com/kansi)

## Git Workflow

We are using github for the whole code base. There are many documents
explaining a _github-workflow_ and we are using a similar one. Basically we
are differentating between two workflows. One for _contributors_ via a pull
request and one for _committers_ with a feature-branch workflow. But first
some words on _commit messages_ and _feature branches_.

### Commit Messages

The git history (or log) is a very important documentation for finding
commits and changes in the software. Because of this reason, it is very
important to write good commit messages. Please follow these easy rules:

- Write an informative headline not longer than about 50 characters.
- It should have a short prefix indicating the general area this patch
  affects, such as `docs:`, `swarm:`, `peer:`, `site:` etc.
- If you have been working on an issue or task, please use the task or issue
  title.
- Include a newline after the title.
- The commit body can be as long as you like, but please ensure that you
    refer to any issues or tasks that relate to this - for example, `closes
    #123 increase the foo force factor`
- Include as much information needed to understand what you have written or
  changed as needed.

### Feature Branches

If you are working on a bigger part, please create a feature-branch first
and work on this branch. The convention we are using for the name is

    feature/name_of_the_branch

The name should be clear and informative. If you are working on an issue or
issue, please use the name of the issue or issue as the name for the branch.

__Example:__

You want to work on the issue [add getopt support for ./swirl
command](https://github.com/skunkwerks/swirl/issues/5). A good name for a
feature-branch would be:

    feature/5-add_getopt_support_for_swirl_command

### Contributor workflow

The first step is to fork the github project to your own account. You are
then able to work on the code base. Then create a feature branch as in
[Feature Branches](#feature-branches). After finishing your work, please
send a [pull request](https://help.github.com/articles/using-pull-requests)
to us. We will then review the code in your pull request, leave comments and
finally accept or reject the pull request.

### Committer workflow

As a committer you can push directly to the main repository but we encourage
you to use the PR workflow shown above for features other than small typos
or fixes. We expect a higher standard from committers, so please follow this
workflow:

1. Create a feature branch for bigger changes, tasks or issues. If it is an
issue the feature branch should be named like: `1912-troubleshooting-guide`
where `1912` is the issue number and `Troubleshooting Guide` the title of
the issue.

2. Please follow these steps for your work:
    - create feature branch from develop
    - work on feature branch
    - clean up feature branch commits (clean up whitespace in the code area)
    - rebase feature branch onto current develop
    - merge feature branch into develop

3. Squash or rebase your work into logical steps - this results in the
following single commits:
    - minimally complete refactoring #1
    - minimally complete refactoring #2
    - minimally complete refactoring #3
    - actual feature/fix, part #1
    - actual feature/fix, part #2

4. Do a final check for whitespace, correct Erlang indentation as needed. If
you have `vim 7.4` or higher installed, you can use `make reindent` for
this.

5. Ensure that `make distcheck`, which removes all unwanted files in the
repo, and resets all listed files to the last commit, before running all
tests, passes successfully.

6. For significant changes, please get a `+1` confirmation from another
project member.

# Documentation

Swirl is based on the reference IETF protocol document, `RFC7574`, at
[RFC7575: Peer-to-Peer Streaming Peer
Protocol](https://tools.ietf.org/html/rfc7574)

The [documentation] for the project itself is still a work in progress; please
[let us know](https://github.com/skunkwerks/swirl/issues/new) if you find
anything unclear or missing.

## Coding Style

Please respect ~80 character line limits, whitespace, and indentation.

Apart from a few odd places, the code is indented using the definitive
Erlang emacs style. You are welcome to use vim of course, and from vim 7.4
onwards, the same erlang indentation should apply as in emacs. There are a
number of useful vim plugins at https://github.com/vim-erlang/ as well as
the canonical `vim-erlang-runtime` plugin if your vim is older than 7.4. Use
the `gg=G` shortcut in vim to re-indent correctly, and check your `.vimrc`
includes `filetype plugin indent on`. There is a handy `make reindent`
target which uses vim's default indentation to clean up your commits.

If you're creating a new file, please copy `./src/template` which includes
all the sensible things like license header, emacs and vim tab settings.

## Testing

The code of the project is tested. So when sending a pull request from your
fork or when adding code as a contributor, please add appropriate tests. A
good place to start is to have a look into the [test
directory](https://github.com/skunkwerks/swirl/tree/develop/test). As a rule
of thumb, put API (exported module functions) into a `common_test` module,
and only use `eunit` for internal module tests, if these cannot be tested
from `common_test` already. There's a template in `./test/template_SUITE` to
help.

Contributions should pass `make distcheck` before being merged to `develop`
or release branches. This cleans the repo, and runs all tests (eunit,
common_test, and dialyzer) checks on your behalf.

## Bug Reports

The project is using the github issue tracker. You can find it at
https://github.com/skunkwerks/swirl/issues . If you think you found a bug,
please have a look into the issues list first. If you can't find a similar
issue, you are welcome to open a new one.

# Become a Contributor

We are very happy to welcome new contributors to the project. To become a
committer, we ask you to fork this project and send a pull request with your
contributions. By doing so on a regular basis, we are eventually happy to
add you as a contributor.

Please ensure that all contributions are your original work, or credited
appropriately, and that all the code *you* write must be under the [Apache
v2 License](http://www.apache.org/licenses/LICENSE-2.0.html).

[documentation]: https://github.com/skunkwerks/swirl/tree/develop/doc

