# BIOU9PC Population Practicals

- **Authors:** [Rose McKeon](http://rosemckeon.co.uk), [Tim Paine](https://www.stir.ac.uk/people/18456)
- **Version:** 0.1.2
- **Licence:** MIT
- **Repo:** [git@bitbucket.org:rose-mckeon/biou9pc-2017.git](https://rosemckeon@bitbucket.org/rose-mckeon/biou9pc-2017.git)
- **Issues:** <rose@rosemckeon.co.uk>

# Based on Ramen

[Ramen](https://github.com/rozeykex/ramen/) controls dev dependecies and sets up tools for compiling javascript files and stylesheets. See the Ramen repo for more details on commands to run after initial cloning and gulp commands available to compile your assets.

# Shiny Apps

The apps themselves are named **practical-01**, and so on. You can make any edits to the functionality of these apps in the contained R scripts, without need for ramen setup or any compiling commands. 

1. **Population growth & regulation:** http://guianaplants2.stir.ac.uk/biou9pc-01/
1. **Matrix population models:** http://guianaplants2.stir.ac.uk/biou9pc-02/
1. **Meta-populations:** http://guianaplants2.stir.ac.uk/biou9pc-03/
1. **Stochasticity & Hunting:** http://guianaplants2.stir.ac.uk/biou9pc-04/
1. **Predation:** http://guianaplants2.stir.ac.uk/biou9pc-05/
1. **Competition:** http://guianaplants2.stir.ac.uk/biou9pc-06/
1. **Food Webs:** http://guianaplants2.stir.ac.uk/biou9pc-07/
1. **Spatially explicit predation:**

## Showcase mode

To enable or disable showcase mode, change `DisplayMode:` in the app DESCRIPTION file. Valid options are: **Showcase** or **Normal**. The later practicals used a more adanced file structure so that showcase mode can be enabled and all the UI and server code should be hidden automatically. Basically, eveything in the root app folder is shown by default, so we've moved some scripts to subfolders. I've also added a custom script that hides ui.R and server.R as these cannot be put into subdirectories. All other R scripts will appear in the showcase.

### Quirks 

Showcase mode adds table wrappers to the app markup and allows code to be displayed alongside the app - reducing screen space without that space following reduced `@media` style rules. Bootstrap `.container` divs are, therefore, innapropriate content wrappers. Use `.container-fluid` instead.

### Tab customisations

All R files in the app root will be displayed except for *ui.R* and *server.R*, which are hidden by a custom script in main.js. The script names all these tabs with css classes, so you can easily hide any tabs - even if the file remains in the app root directory.

# Version control

Some git commands to get you started:

- `$ git status` See what you've changed that is ready to commit/add.
- `$ git commit -am 'your message'` Commit all modified files git is already aware of, with an message explaining your changes.
- `$ git add -A ./` Stage everything (modifications, new files, new folders) from the current directory to your next commit.
- `$ git commit -m 'your message'` Commit everything that is staged (ie: previously added).
- `$ git pull` Pull changes from the remote branch (may require you to set upstream branch. Git will prompt you if extra config comands are required first time).
- `$ git push origin master` Push your changes to the remote branch (you can simplify this to `git push` Try it and git will tell you how to set gitconfig variables to allow it).

> You can also just enable the Git pane in RStudio and use the graphical interface.

# Server updates

The server is updated by pulling changes:

`$ cd /srv/shared/biou9pc-2017 && git pull`

And by symlinking to new apps, eg:

`$ ln -s /srv/shared/biou9pc-2017/practical-01 /srv/shiny-server/biou9pc-01`

Manage the server with:
`sudo service shiny-server' `followed by `stop start restart `or` status`


## R Packages

Packages used in apps will need to be installed manually on the shiny server using:

1. `$ R`
2. `> install.packages("package", lib="/usr/lib/R/site-library")`

---

# Markup and CSS

These appps make use of [Bootstrap](http://bootstrapdocs.com/v3.3.6/docs/getting-started/), which is loaded by default in Shiny apps. We have replaced default script and style includes with recompiled, compressed versions but the end result is the same; all the Bootstrap classes and utilities are available to you. So, keep this in mind when coding UI elements.

## The Bootstrap grid

The main reason to use Bootstrap, above all others, is the grid. Grid classes on block level elements replaces the old fashioned approach to markup layout via tables. In a way, it is similar to using <tr> and <td>, but it grants greater flexibility, poses less display issues across browsers, and allows us to easily control column layouts for different screen sizes.

**The basics are:**

- **Each row can be filled with a maximum of 12 columns** (we actually cutomise the grid to expand this to 22 for the matrix in P7).
- **Columns are full width by default** until you specify otherwise (ie a width of 12)
- **Letter codes in column classes define the screen size where column sizing begins to take effect.** For example a column with a class "col-xs-12" will always be 12 wide, but one with the class "col-md-6" will change to be half the width of the screen for medium (md) screens and above.
- **You can apply multiple column classes to one column container** so it will behave differently for different screens. The most basic approach is to leave extra small (xs) and small (sm) screens with full width containers, then allow larger screens to stack your columns in a more table like manner. **You can do this simply by adding the class for the larger screen** "col-lg-4" (or whatever), as every screen size below this will automatically be rendered as full width.

**Loads more info on more detailed grid layout options here:** http://bootstrapdocs.com/v3.3.6/docs/css/#grid

> I can't wait for Shiny to update to the latest Bootstrap as the grid has become loads more advanced. I've included the most recent I could that didn't cause any issues.

A new little edit.
