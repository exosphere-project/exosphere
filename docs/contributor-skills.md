# Contributor skills

Note: You don't need all the skills mentioned below to start contributing to Exosphere.
We aim to create a welcoming environment where people can learn by doing.

## Areas with corresponding skills requirements

| Skill↓   \|  Area→  | User interface | Instance configuration | OpenStack interactions | Browser tests | Documentation |
|---------------------|----------------|------------------------|------------------------|---------------|---------------|
| Elm Web Development | Essential      | Optional               | Essential              |               | Optional      |
| Linux and Ansible   |                | Essential              | Optional               |               | Optional      |
| Cloud APIs          | Optional       | Optional               | Essential              |               | Optional      |
| Python BDD Testing  |                |                        |                        | Recommended   |               |
| Interaction Design  | Recommended    |                        |                        |               |               |
| Git and GitLab      | Recommended    | Recommended            | Recommended            | Recommended   | Optional      |
| Markdown            |                |                        |                        |               | Essential     |
| A+ Written English  | Essential      | Optional               | Optional               | Recommended   | Essential     |

**Key:**

- *Essential*: Must learn to contribute effectively
- *Recommended*: Can get started without but may need later
- *Optional*: Could be helpful some of the time
- *(Blank)*: Not applicable


## Skills, with links for learning more

- Elm Web Development
  - [An Introduction to Elm | The Official Elm Guide](https://guide.elm-lang.org/)
  - [Debugging with Elm | elm-lang.org](https://elm-lang.org/news/the-perfect-bug-report) (written for older version of Elm, but still useful)
  - [Learn Web Development | MDN](https://developer.mozilla.org/en-US/docs/Learn)
  - [Hypertext Transfer Protocol (HTTP) | MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP)
  - [What are browser developer tools? | MDN](https://developer.mozilla.org/en-US/docs/Learn/Common_questions/Tools_and_setup/What_are_browser_developer_tools)
- Linux and Ansible
  - [The Linux command line for beginners | Ubuntu](https://ubuntu.com/tutorials/command-line-for-beginners)
  - [Get Started | Ansible](https://www.ansible.com/resources/get-started)
- Cloud APIs
  - [OpenStack Tutorials | Ubuntu](https://ubuntu.com/tutorials?topic=openstack)
- Python BDD (Behavior-Driven Development) Testing
  - [Python for Beginners | Python.org](https://www.python.org/about/gettingstarted/)
  - [behave Tutorial - Behavior-Driven Development (BDD), Python style | ReadTheDocs](https://behave.readthedocs.io/en/stable/tutorial.html)
  - [behaving - Behavior-Driven Development (BDD) for multi-user web/email/sms applications | GitHub](https://github.com/ggozad/behaving)
- Interaction Design
  - [Best Interaction Design Courses & Certifications | Coursera](https://www.coursera.org/courses?query=interaction%20design)
  - [Learn Interaction Design | Codecademy](https://www.codecademy.com/learn/learn-interaction-design)
  - [Learn Interaction Design with Online Courses, Classes, & Lessons | edX](https://www.edx.org/learn/interaction-design)
- Git and GitLab
  - GitLab is a very similar service to GitHub; if you're familiar with one, using the other is quite straightforward. 
  - [Git | GitLab](https://docs.gitlab.com/ee/topics/git/)
  - [Learn GitLab with tutorials | GitLab](https://docs.gitlab.com/ee/tutorials/)
- Markdown
  - [Markdown Cheatsheet | GitHub](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet)
  - [GitLab Flavored Markdown | GitLab](https://docs.gitlab.com/ee/user/markdown.html)
- A+ Written English
  - This includes:
    - The ability to write text that does not contain obvious mistakes or confuse the reader.
    - The ability to communicate technical concepts succinctly, with an awareness of the likely audience.
    - Consistent use of grammar, punctuation, and capitalization.
  - It is most important for:
    - Text displayed in Exosphere's user interface.
    - Documentation.
  - It is also helpful (though less critical) in code.
  - [The Elements of Style](https://www.gutenberg.org/files/37134/37134-h/37134-h.htm)
  - [Subject-verb agreement](https://owl.purdue.edu/owl/general_writing/grammar/subject_verb_agreement.html)

## On Git Skills

Git is an advanced version control system, so there are different levels of git skill.

To make your first Exosphere contribution, you only need basic familiarity with the `clone`, `switch`, `add`, `commit`, and `push` commands. Our [quick start](../contributing.md#quick-start-for-new-contributors) section shows each git command to run. This is about the same skill level you'd need to host a small personal project  on GitHub.

To work collaboratively on major changes to Exosphere's codebase, you will need deeper git knowledge. This includes knowing, at least roughly:

- How to write effective commit messages
- How to structure and order commits so that code reviewers can follow a multi-commit change
- How to manage multiple git remotes (one for each colleague)
- The difference between `git fetch`, `git merge`, and `git pull`
- The difference between a fast-forward merge and a two-parent merge
- How to rebase changes
- What merge conflicts are, and how to resolve them
- Optional but helpful, Git's [basic data structure](https://eagain.net/articles/git-for-computer-scientists)