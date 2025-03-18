Layers of Reproducibility
================
Jonas Schöley & Egor Kotov

## Layer 0: Aspirational reproducibility

> The setting: After 6 months of silence, you finally got the reviews for your paper. It's an R&R with many requests for additional analyses. You open your project folder and can't reproduce your old results. It's hard to remember how everything worked. You wish you had documented and organized your work better.

## Layer 1: Personal reproducibility

### Topics

1. Project organization
	1. What is a project?
	2. Folder structure, templates, and naming conventions
	3. Research notes
2. Code organization
	1. The analysis pipeline
	2. Defining inputs and outputs
	3. Configuration files
	4. Comments

### Examples

- [Example 1-1](https://github.com/jschoeley/openscience25/tree/main/layer1-personal/example_1-1-project)
- [Project template](https://github.com/jschoeley/openscience25/tree/main/layer1-personal/example_1-3-template)
- [Project diary](https://github.com/jschoeley/openscience25/blob/main/layer1-personal/example_1-2-diary/diary.md)
- [Paper to reproduce](https://www.demographic-research.org/articles/volume/44/19)

## Layer 2: Communal reproducibility

### Topics

1. Code repositories
	1. The git version control system
	2. Github/Gitlab/Codeberg as online code repositories
	3. Keeping private things private
	4. Code documentation
	5. Large file
2. Data repositories
	1. Zenodo
	2. Integrating Zenodo into your code
	3. Integrating Zenodo and Github
	4. "I can’t share my data"
	5. Data documentation
3. Advertisement of your shared resources

- [Example 1-2](https://github.com/jschoeley/openscience25/tree/main/layer2-communal/example_2-1)

## Layer 3: Computational reproducibility

### Topics

1. Workflow automation
	1. Disadvantages of a single script. Namespace issues, package conflicts, etc.
	2. The `targets` R package for defining an analysis pipeline
		- Computational isolation of each target step
2. Reproducing R-language dependencies (with renv, mention rix as an option)
	1. Listing dependencies in a project
	2. Package version management
3. Containers for near complete reproducibility
	1. The problem of system level dependencies
	2. The container solution
	3. Using other peoples containers
	4. Building your own

### Exercises

- [Egors Repository](https://www.ekotov.pro/2025-mpidr-open-science-reproducible-workflows/)
- [Excercise 1: renv](https://www.ekotov.pro/2025-mpidr-open-science-reproducible-workflows/1-r-packages.html)
- [Excercise 2: targets](https://www.ekotov.pro/2025-mpidr-open-science-reproducible-workflows/2-analysis-pipeline.html)