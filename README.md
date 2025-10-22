# viva.el.birdos
A repository to enable the VEB community to share code for baseball, specifically St Louis Cardinals, analysis.  For folks wanting to contribute: 


## for working in R:
1. Create a github account
2. Download the github desktop software, install on your computer
3. Download R, install
4. Download Rstudio, install
5. Open github desktop, 'clone a repository' and find cbroeckl/viva.el.birdos
    - in upper left, find 'current repository', and click 'add' then 'clone repository'
    - find cbroeckl/viva.el.birdos, clone.  
    - this downloads all content to your local drive. ('my documents/github' on windows, by default)
    - in your local repository, i suggest creating a new directory for your files, called by your VEB username or your name. 
    - add your R code files to the existing files set, ideally in your personal subdirectory you just created.
    - once you are comfortable sharing you files publicly, 'push' your code back to the repository
       - in github destop, find your edited new file, fill in description in lower left of window (required)
       - 'commit' edit (just below description you just entered)
       - 'push origin' to copy your local code to the viva.el.birdos repository.  

## For working in Python: 
This python project uses uv to manage packages. If you don't have uv installed
on your machine, you'll need to do that. On a Mac, that would be something like 
`brew install uv`. On other operating systems, you're on your own. (which is to 
say, please update the docs accordingly)

```bash
# To install the necessary packages
uv pip install -r requirements.txt
# to activate the virtual environment
source .venv/bin/activate
```

## overview
the idea here is that we can learn from each other.  Note that if you edit code from viva.el.birdos locally, it stays local, UNLESS you push it back to the repository.  any edits you 'push' will replace the versions on the viva.el.birdos repository.  Github does use versioning, which means any chance can be rolled back, but do be mindful that editing code collectively means you may be editing other people's code, and overwriting other documents.  if you aren't sure you want to do that, save the file as a new file name, which prevents issues.  We will be learning as we go.
