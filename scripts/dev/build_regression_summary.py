from sys import argv

summary_input_md_file = argv[1]
summary_output_js_file = argv[2]
matrix_os = argv[3]
github_sha = argv[4]
github_run_id = argv[5]
artifact_url = argv[6]

with open(summary_input_md_file) as md:
    md_contents = md.read()

fixed_up_contents = f"""
### :warning: Regressions detected on {matrix_os} for commit {github_sha}

{md_contents}

 - [View Results](https://github.com/NREL/EnergyPlus/actions/runs/{github_run_id})
 - [Download Regressions]({artifact_url})
"""

with open(summary_output_js_file, 'w') as js:
    js_contents = f"""
module.exports = ({{github, context}}) => {{    
    github.rest.issues.createComment({{
        issue_number: context.issue.number,
        owner: context.repo.owner,
        repo: context.repo.repo,
        body: `{fixed_up_contents}`
    }})
}}
"""
    js.write(js_contents)
