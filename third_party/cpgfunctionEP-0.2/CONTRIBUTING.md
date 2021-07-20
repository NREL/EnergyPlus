# Contributing to cpgfunction

## Branching

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
