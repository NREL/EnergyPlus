#!/bin/bash

# call with the PR number as the only command line argument

# get the PR num from the command line argument
PR_NUM=$1

# the GraphQL Project ID can be retrieved from a given organization's project, where the URL is:
#  https://github.com/orgs/ORGANIZATION/projects/SOME_PROJECT_NUMBER/views/2
# and the associated call to graphql is:
#gh api graphql -f query='
#  query{
#    organization(login: "ORGANIZATION"){
#      projectV2(number: SOME_PROJECT_NUMBER) {
#        id
#      }
#    }
#  }'
# TODO: Just specify the project organization and number and get the graphql ID in here
PROJ_ID=PVT_kwDOAB0YcM4AEWD7
echo "Using PR Num as ${PR_NUM} and project ID as: ${PROJ_ID}"

# get the current PR ID based on the this checkout
CONTENT=$(gh pr view "$PR_NUM" --json 'id' --jq '.id')
echo "Found PR node ID as: ${CONTENT}"

# use the gh api command line to act on the Projects-v2 API and add the PR as a new card
# should also add more arguments for the column to use, etc.
gh api graphql -f query="
  mutation {
    addProjectV2ItemById(input: {projectId: \"${PROJ_ID}\" contentId: \"${CONTENT}\"}) {
      item {
        id
      }
    }
  }"
