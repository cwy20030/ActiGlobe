Guidelines and Contribution
================
The ActiGlobe Team

- [Code of Conduct :page_facing_up:](#code-of-conduct-page_facing_up)
- [Citation :pencil2:](#citation-pencil2)
- [Ways to Contribute :two_hearts:](#ways-to-contribute-two_hearts)
  - [Ask Questions ⁉️](#ask-questions-interrobang)
  - [Improve Documentation :notebook:](#improve-documentation-notebook)
  - [Bug Report :bug:](#bug-report-bug)
  - [New Feature and Ideas :bulb:](#new-feature-and-ideas-bulb)
- [General Guideline on Liscening, Documentation and Code Contribution
  :clipboard:](#general-guideline-on-liscening-documentation-and-code-contribution-clipboard)
  - [Container :floppy_disk:](#container-floppy_disk)
  - [Licensing :card_index:](#licensing-card_index)
  - [Documentation and Website
    :black_nib:](#documentation-and-website-black_nib)
  - [General Pointers for Coding and Documentation
    :computer:](#general-pointers-for-coding-and-documentation-computer)

<b> First of all, thank you for considering contributing to ActiGlobe
</b>! :pray: It is people like you that make it rewarding for us - the
project maintainers - to work on ActiGlobe. 😊 This project thrives on
community contributions. Whether it is bug report, suggesting new
features, improving documentation, contributing code or providing aid in
developing new modules, we are emensely grateful for the time and effort
of our contributors.

ActiGlobe is an open-source project maintained by people who care about
advancing research and building reliable tools.

- Repository: <https://github.com/cwy20030/ActiGlobe>
- Issues: <https://github.com/cwy20030/ActiGlobe/issues>
- New issue: <https://github.com/cwy20030/ActiGlobe/issues/new>
- Email: <chun.william.yao.cnmtl@ssss.gouv.qc.ca>

## Code of Conduct :page_facing_up:

This package is released with a Contributor Code of Conduct. By
contributing, the contributor (you) agrees to uphold its principles and
the Affero General Public License. Contributors, authors and maintainers
of this project are also expected to be respectful in all forms of
communication and to maintain/foster the collaborative environment.

As authors and maintainers, we are committed to making participation in
this project a harassment-free and inclusive experience for everyone.
Any form of harassment or discriminatory behavior, such as the use of
derogatory comment or unprofessional conduct will not be tolerated.
Project maintainers retain the right and responsibility to remove, edit,
or reject comments, commits, code, wiki edits, issues, and other
contributions that are not aligned to this Code of Conduct. Contributors
who do not follow the Code of Conduct may be removed from the project
team. In the instances of abusive, harassing, or otherwise unacceptable
behavior, we encourage affected parties to report the incident(s) using
the `Report content` button under the `...` menu on the specific GitHub
issue or pull request.

## Citation :pencil2:

To support the continuation and the growth of ActiGlobe, it is important
for manuscript authors (i.e., users) to cite ActiGlobe when used in
their research. Proper citation also helps other researchers discover
ActiGlobe and understand its applications. Users may obtain the citation
information by running the following command in R:

``` r
citation("ActiGlobe")
```

## Ways to Contribute :two_hearts:

There are many ways to contribute to this project. The easiest way to
contribute is to spread the words about ActiGlobe through social media
or word of mouth. Beyond that, you can also contribute by asking
questions, suggesting new ideas, reporting bugs, improving
documentation, or contributing code. Most of these inquiries can and
should be done by submitting an issue using the template through GitHub.
If you are new to GitHub, please refer to the [GitHub Guide for
Beginners](https://guides.github.com/activities/hello-world/) for help.
For private inquiries or sponsorship, please contact the maintainer
directly by email.

### Ask Questions ⁉️

When encountering difficulties: - The first step should be to review the
documentation to see if the question is addressed. - If not, open an
issue on GitHub. While we cannot provide individual user support, we
will do our best to respond, as questions often highlight areas where
documentation can be improved.

### Improve Documentation :notebook:

While we strive to provide clear and comprehensive documentation, there
is always room for improvement. If you notice a typo, unclear
explanation, or missing example in the documentation, please report it
on GitHub by opening a new issue using the documentation improvement
template.

### Bug Report :bug:

When encountering an error (colloquially known bug), please report it on
GitHub by opening a new issue using the bug report template. A clear and
reproducible report helps us resolve issues more quickly. We also
recommend the use of reprex package to create a reproducible example.

### New Feature and Ideas :bulb:

Have an idea for a new feature or a function? - First, check the
documentation and issue tracker to see if it has already been
discussed. - If not, open a new issue on GitHub. Please describe your
idea clearly, keep the scope focused, and explain how it would work. See
the following section for more information about how to contribute to
ActiGlobe.

#### New Feature Request and Idea Contribution Process

There are two ways to contribute: requesting new features or
contributing new ideas directly in code. The primary distinction between
the two is the level of effort required from the contributor.

Requesting a new feature typically involves less effort, as it only
requires describing the idea and its intended functionality. While we
are committed to reviewing and considering all feature requests, we
cannot guarantee that every request will be implemented. Nevertheless,
we welcome any and all suggestions for new features. To request a new
feature, please open a new issue on GitHub using the feature request
template. Be sure to describe your idea clearly, keep the scope focused,
and explain how it would work.

#### New Idea Contribution Directly in Code :crown:

In contrast, contributing new ideas directly in code demands more
effort, as it involves implementing the idea and ensuring it integrates
seamlessly with the existing codebase. We encourage potential
contributors who wish to implement new ideas to contact us beforehand to
discuss the feasibility and alignment with the project’s goals. For this
type of inquiry, contributors should first issue a request using the new
idea template. Once submitted, one of the authors/maintainers will
review the request and provide feedback. If the idea is deemed suitable
for implementation, the contributor can then proceed to develop the
code, adhering to the project’s coding standards and guidelines.

To acknowledge the significant effort involved in contributing new ideas
in code, we offer recognition to contributors whose ideas are
successfully integrated into the project. :trophy: Depending on the
contribution, the contributor may be invited to become a collaborator on
the project. A contributor may also be recognized in the project’s
documentation or release notes, highlighting their contribution to the
project’s development. We believe that recognizing contributors is
essential for the Open Source community, as it not only fosters a sense
of community but also encourages further contributions and
collaboration.

When contributing new ideas in code, please make sure to fork (i.e.,
copy) the repository and create a new branch for your changes. This
practice helps maintain a clean and organized codebase, allowing for
easier collaboration and review. Once the code is ready, submit a pull
request for review. We ask all contributors to follow the recommended
workflow and use the pull request template provided below to facilitate
the review process. For more details about the general coding and
documentation guidelines, please see the next section.

##### Work Flow for Code Contribution :muscle:

To fix bugs or add new functionality:

- Review the issue list and comment on items you wish to work on.
- Create an issue before coding.

After receiving the ‘go-ahead’ from the authors or maintainers,

- Fork the repository and clone it locally.
- Open the RStudio project file (`.Rproj`).
- Make your changes.
- Write and test your code (unit tests are encouraged).
- Document your code with `roxygen2`.
- Check licencing documentation.
- Run `devtools::check()` and aim for zero errors and warnings.
- Commit and push your changes.
- Submit a pull request.

We follow the GitHub flow for development. If you have previously forked
the project, please update your fork with the latest changes before
starting new work.

## General Guideline on Liscening, Documentation and Code Contribution :clipboard:

As with most R package development, contributions to `ActiGlobe` are
encouraged to familiarize themselves with
[guidelines](https://r-pkgs.org/) proposed by Wickham and Bryan on
package development in R. To ensure consistency and maintainability of
the codebase, all code contributions, including bug fixes, new features,
and documentation improvements, must adhere to the project’s coding
standards and guidelines. Contributors are expected to follow best
practices for R programming, including proper documentation using
`roxygen2`, writing unit tests with `testthat`, and adhering to the
project’s code style. Before submitting a pull request, contributors
should ensure that their code has been thoroughly tested and reviewed
for quality and consistency. By following these guidelines, we can
maintain `ActiGlobe` and ensure that it remains a reliable and useful
tool for the community.

### Container :floppy_disk:

We strongly encourage contributors to create a container, such as
RStudio project file (`.Rproj`) and keep the `.Ristory` when coding for
`ActiGlobe`.

### Licensing :card_index:

`ActiGlobe` is licensed under the Affero General Public License (AGPL)
v3.0. By contributing to this project, you agree that your contributions
will be licensed under the AGPL v3.0. For more information about the
AGPL v3.0, please refer to the [GNU
website](https://www.gnu.org/licenses/agpl-3.0.en.html). Any
modification or addition made to the codebase will be subject to the
same license, ensuring that the project remains open and accessible to
all users.

<u>All contributors must agree to the terms of the AGPL v3.0 before
submitting any contributions.</u> If you have any questions or concerns
about the licensing of your contributions, please contact the authors or
project maintainers for clarification.

### Documentation and Website :black_nib:

Function documentation should be written in
[roxygen2](https://roxygen2.r-lib.org/) comments within the R/
directory. All user‑facing functions (i.e., the function that users can
execute directly through the R shell) should have a clear and concise
title and description, along with examples of how to use it. For
functions involving signal processing or statistical analysis,
contributors should add additional documentation about the workflow, the
rationale of design or essential mathematical processes to the detail
section.

### General Pointers for Coding and Documentation :computer:

We understand that like handwriting, we all have our coding style, which
can also change by day. It is; therefore, unrealistic to expect everyone
to have just one coding style. In general, we are flexible with
contributors’ coding style and design strategy. Nevertheless, we
encourage contributors to follow the guidelines listed below. These
guidelines were designed to ensure readability and documentation, while
helping the maintainers reviewing the code. Afterall, it can be
difficult for maintainers to simultaneously review the code while
learning a completely new coding style unique to the contributor.

#### Code Style :pencil:

- <b>Temporary Variable or Function Naming </b> :id:
  - Consider adding at least one <b> MAJESTIC </b> letter or assign
    acronym that is not commonly used in other functions. Most functions
    in R pacakges are written in small case. Therefore, sometimes a
    small case name is already assigned as a function from an external
    function. This can cause a problem because R can get confused. To
    avoid this type of issue, we recommend contributors to avoid using
    small case naming system unless it is uncommon jargon.

  - If a function aims to execute an action, such as `write` or `save`
    an object, contributors may use a small verb-based naming system
    followed with `.` and the type of output object (e.g., `write.act`,
    as export actigrpahy data by day).
- <b> Spacing </b> :mag_right:  
  ActiGlobe uses `pkgmatch` style that emphasizes readability through
  consistent use of whitespace. Just as text is easier to read when
  words are separated, code is clearer when elements are spaced
  appropriately. For example:

<!-- -->



    this <- function ()

We understand that this is different from the typical `tidyverse`
strategy. So it is not essential to follow this rule when drafting and
testing the code. Contributors may simply use the
[styler](https://github.com/r-lib/styler/blob/main/CONTRIBUTING.md) and
[spaceout](https://github.com/ropensci-review-tools/spaceout) packages
to re-format the code before submitting a pull request. This can be done
easily using



    styler::style_pkg (style = spaceout::spaceout_style)

#### Documentation Guidelines

Contributors may consider using this template when coding.


    #' @title Short, clear description of the function
    #'
    #' @description
    #' A longer explanation of what the function does. Include context, 
    #' assumptions, and any important details about usage.
    #'
    #' @details
    #' Optional section for more in‑depth technical notes, algorithmic 
    #' explanations, or references.
    #' Use \eqn{} or \deqn{} whenever appropriate, including the use of non-english letter.
    #' 
    #' @import package_name
    #' @importFrom package_name function1 function2  
    #' 
    #' @param x Description of the first argument (type, expected input).
    #' @param y Description of the second argument.
    #' @param ... Additional arguments passed to methods. 
    #'
    #' @returns
    #' A simple statement for what the function returns (object type, structure, meaning).
    #' 
    #' For list object, signal processing, or statistical computation, please add \itemize{} 
    #' for return documentation whenever appropriate.
    #' \itemize{
    #'   \item returned_object Parameters specified in the model 
    #' }
    #'
    #' @seealso 
    #' \code{\link[stats]{lm}}  \code{\link{CosinorM.KDE}}
    #'
    #' @examples
    #' # For time-consuming functionality or demonstrations of an expected failed 
    #' # execution, please add \dontrun{ } around the example.
    #'
    #' # With named arguments
    #' MyFunction (x = 10, 
    #'              y = 20)
    #'
    #'
    #' # Avoid far too simple documentaion like this since users may be unfamiliar 
    #' # with the concept of coding 
    #' MyFunction (1, 2)
    #' 
    #' # consider adding some pro-tip like
    #' ## Pro-tip: Typing can be time conssuming. So when coding, use the TAB button 
    #' ## on the keyboard so you do not need to type every word. Try typing: My then 
    #' ## press TAB on your keyboard, often it finish writing the function name for you.
    #'
    #' @keywords Acronym tag_relevant_to_the_function
    #' @export

    MyFunction <- function(x, y, ...) {
        
    ## For large user-facing functions (i.e., totoal code row >150 including the 
    ## documentation), "------------" is essential for code organization. Wrap 
    ## related codes together for each step  to allow visualization of the workflow 
    ## through `Outline` or `Index` in an UI.    
        
      # Step 0 Fail_Control_for_input_arguments -----------------------
      ## This include any check for input varaible class and value
      ## For checkpoint of input object class (a technical terminology for data type in R), please use `inherits()` function
      
      if (!inherits (x, "numeric")) { x <- as.numeric (x) }
        
        
      # Step 1 Extraction of Essential Parameters -----------------------  
      ## For non-vector input object, please extract essential parameters from the input object here.
      ## e.g., for data.frame input object, please extract the relevant column(s) here and assign them in the code below
      
      IANA <- get0 ("IANA", envir = asNamespace ("ActiGlobe" )) # Consider adding note 
      TZ <- IANA$TZ_Code 
        
         
      if ()   
        
      # Step abc Document the primary purpose of the code chunk -----------------------    
      ADS <- x + y  
      ABC <- x/2
      EFG <- y/3
      
      
      
      # Step xyz Generate Output ------------------
      Out <- list ( 
      
      NAME_TO_DESCRIBE_FIRST_OUTPUT = ADS,
      NAME_TO_DESCRIBE_SECOND_OUTPUT = ABC,
      NAME_TO_DESCRIBE_THIRD_OUTPUT = EFG
      
      )
      
      
      return (Out)
    }

#### Filing Strategy :floppy_disk:

There are two general rules when it comes to code filing.

- All <b>user-facing functions</b> should be stored as their individual
  files.

- <b>Helper or Internal Functions</b> :nut_and_bolt:

  - <u>Single-use helper function</u>: if a helper function is
    specifically designed for a specific user-facing function only, then
    it should be added right after the user-facing function, as
    demonstrated below.

  - <u>General helper function</u>: if a helper function is (meant to
    be) used by multiple user-facing functions or other helper or
    internal functions, then they should be saved as a file of their
    own. If multiple related helper functions are created, contributors
    propose filing strategy to the maintainers.

<!-- -->


    #' @title Short, clear description of the function
    #'
    #' @description
    #' A longer explanation of what the function does. Include context, 
    #' assumptions, and any important details about usage.
    .
    .
    .
    #' @export

    Primary_Function <- function(x, y, ...) {
        .
        .
        .
        
        b <- OneMore(x) ## Use of the helper function
        .
        .
        .
      
      return (Out)
    }


    # Pre-defined helper functions -------------------------------
    #' @title A brief yet conscise description of the helper function
    #' @noRd
    OneMore <- function (a) { 

        Out.a = a + 1

        return(Out.a)
    }

- <b>S3</b>, <b>S4</b> and <b>R6</b>: When coding using these
  object‑oriented systems, it is essential for the contributors to
  clearly stated the differences in the functions. While these object
  generators are flexible, they require special care for documentation.
  For these type of functions, we recommend contributors to create a
  schematic design chart when coding. These charts can be helpful when
  documentation and code revision.

#### Pull Request Template 🪢

When submitting a pull request, please use the following template to
ensure that all necessary information is provided.


    # Description
    Explain the changes you made and why.  

    Fixes # (issue number)

    ---

    # Type of Change
    - [ ] Bug fix  
    - [ ] New feature  
    - [ ] Documentation update  

    ---

    # Checklist
    - [ ] Code follows project guidelines  
    - [ ] Tests added/updated  
    - [ ] Documentation updated  
