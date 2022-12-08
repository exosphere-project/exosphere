# MkDocs Site

Exosphere has a website that is built with the [MkDocs](https://www.mkdocs.org) static site generator. It is rendered mostly from the collection of Markdown files in the Exosphere git repository.

## Developing the Site

Browse to the `mkdocs/` directory and run these commands to set up your development environment:

```
virtualenv venv
source venv/bin/activate
pip install -r requirements.txt
```

Then, with the virtual environment activated, run the mkdocs development server:

```
mkdocs serve
```
Now you can view the live website at `http://localhost:8000`, and when you save changes to the source files, your browser should automatically refresh.

If you already have another process bound to port 8000, you can ask MkDocs to serve on an alternate port:

```
mkdocs serve -a localhost:31337
```

## Building the Site Manually

Activate your virtual environment and run:

```
mkdocs build
```