# Track

Track is a very simple command line tool to track your time with a
backing [Redmine][redmine] server.


## Example

Here is a small example of how to use it. 

```bash
$ track list
Hours: 0.0
$ track
Issue: 1337
Comment: Did some stuff
Time spent: 0.25
Activity (choose one, * is default):
     8 Design
(*)  9 Development
> 
$ track list
Hours: 0.25
0.25 Some Project Did some stuff
```

In this example you needed to set the time, you spent on something
manually. If you, however modify the comment a little like this:

```bash
Comment: 9:00 - 10:00 Did some stuff
```

The duration you spent on something will then be calculated
automatically.


## Getting started

You should be able to download a binary from the releases
page. [Here][latest] is a direct link. I am not well versed in what it
takes to provide a fully functional static binary, that works on all
platforms, so if it doesn't work for you, you need to compile
manually.

First, you need to install [stack][stack]. Then you can clone this
directory to your computer and 

```bash
path/to/track $ stack setup && stack build --copy-bins
```

After this you should be able to use it.


## Motivation

Redmine is a great tool to track your time, especially when it relates
to a certain issue or project. However, navigating its user interface
to add a new time entry for a known issue is annoying at best. That is
why this command line tool exists.


[redmine]: https://www.redmine.org/
[stack]: https://docs.haskellstack.org/en/stable/README/
[latest]: 
