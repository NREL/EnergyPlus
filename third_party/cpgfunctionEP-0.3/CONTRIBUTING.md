# Contributing to cpgfunction

## Git

### Branching

To contribute code please first start an issue if one if not already started. 
Then:
- Branch from the most current `master` branch:
```
git checkout -b <branch_name>  # checkout a branch locally from master
git push -u origin <branch_name>  # push to your remote that you have a new branch
git branch --set-upstream-to=<remote_name>/<branch_name> <branch_name>
```
- Commit your changes and push them up
```
git add <file_name>
git commit -m "Type 50 character message here"
git push origin issuexx_ShortDescription
```
- Create a pull request
  
- Document the changelog for your issue

The branch will then be merged into master once it has been reviewed. After the
branch is merged (which will subsequently close the pull request and related
issue(s)): 
- Delete your remote and local branches (proceed with caution) 
```
git push -d <remote_name> <branch_name>
git branch -d <branch_name>
```

### Committing

#### General Rules
- Commit messages must have a subject line and may have body copy. These must 
  be separated by a blank line.
- The subject line must not exceed 50 characters
- The subject line should be capitalized and must not end in a period
- The subject line must be written in imperative mood (Fix, not Fixed / Fixes 
  etc.)
- The body copy must be wrapped at 72 columns
- The body copy must only contain explanations as to what and why, never how. 
  The latter belongs in documentation and implementation.
  
#### Example
This is an example of a complete commit message that adheres to this standard. 
Parts of this are optional, so read on.

```
Summarize changes in around 50 characters or less

More detailed explanatory text, if necessary. Wrap it to about 72
characters or so. In some contexts, the first line is treated as the
subject of the commit and the rest of the text as the body. The
blank line separating the summary from the body is critical (unless
you omit the body entirely); various tools like `log`, `shortlog`
and `rebase` can get confused if you run the two together.

Explain the problem that this commit is solving. Focus on why you
are making this change as opposed to how (the code explains that).
Are there side effects or other unintuitive consequences of this
change? Here's the place to explain them.

Further paragraphs come after blank lines.

 - Bullet points are okay, too

 - Typically a hyphen or asterisk is used for the bullet, preceded
   by a single space, with blank lines in between, but conventions
   vary here

If you use an issue tracker, put references to them at the bottom,
like this:

Resolves: #123
See also: #456, #789
```

Here is [torvalds](https://github.com/torvalds/linux/pull/17#issuecomment-5661185)
addressing the 50/72 rule.

#### Subject Line Standard Terminology

First Word | Meaning
--- | --
Add | Create a capability e.g. feature, test, dependency.
Cut | Remove a capability e.g. feature, test, dependency.
Fix | Fix an issue e.g. bug, typo, accident, misstatement.
Bump | Increase the version of something e.g. dependency.
Make | Change the build process, or tooling, or infra.
Start | Begin doing something; e.g. create a feature flag.
Stop | End doing something; e.g. remove a feature flag.
Refactor | A code change that MUST be just a refactoring.
Reformat | Refactor of formatting, e.g. omit whitespace.
Optimize | Refactor of performance, e.g. speed up code.
Document | Refactor of documentation, e.g. help files.

### Pushing

This repository runs tests via CI/CD upon each push. Multiple commits can be 
made prior to pushing. Currently, the servers for running the tests are owned
by Github. Runtime can be expensive, so the goal is to someday move the tests
to machines owned by the developer(s). 

## Changelog

The cpgfunction library keeps a 
[changelog](https://github.com/j-c-cook/cpgfunction/blob/master/CHANGELOG.md)
so that changes upon each release are transparent and easily understood. Prior 
to a pull request being accepted, all changes must be marked in the changelog. 
The changes should fall under one of the following categories:

- New features - for new features
- Enhancements - for improvements made to code performance and functionality
- Maintenance - for tidying code
- Changes - for changes in functionality of the code
- Deprecates - for soon-to-be removed features
- Removes - for removed features
- Fixes - for any bug fixes

## Versioning

This library makes use of [Semantic Versioning](https://semver.org/).
