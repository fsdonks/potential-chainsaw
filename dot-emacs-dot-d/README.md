## An Example Emacs Setup For Clojure Hacking

### Intro

This folder contains a "starter" setup for Emacs to do Clojure
development, for people who need to get a jump start (i.e., for people
who are not so familiar with how to setup Emacs for this).

### Directions

1. **Copy this example directory into the right place.**  Emacs will
   find its (user) startup files in the directory `.emacs.d` right
   under your home directory; hence, copy the folder `dot-emacs-dot-d`
   there, like this:

   ```
   $ cp -R dot-emacs-dot-d ~/.emacs.d
   ```

   or make the same thing happen in the file manager (e.g., use
   Windows Explorer).

2. **Start Emacs.**  The first time you start Emacs, it should give you
   a blank window and it will seem hung.  Don't worry.  Emacs knows
   you don't yet have the newest versions of all the packages you will
   need for this very setup; so, it is now downloading them.  Give it
   a few minutes.  (It actually took just less than a minute to
   download them on a decent internet connection for me, to give you
   an idea for how long it might take you.)

   By the way, you might see error(s) like the following appear (in a
   pane) during the install of the `magit` package.

   ```
   magit-wip.el:33:1:Error: Searching for program: No such file or directory, git
   ```

   Not to worry.  This is because Emacs/magit doesn't know where your
   git executable is located (yet).  This can be fixed/adjusted later.
   (Hint: look for the setting `magit-git-executable` in `init.el`.)

3. **Profit.**  Restart Emacs (for good measure).  You should get a
   faster startup this time.  Navigate to a Clojure project and open
   the `project.clj` file.  Now, do `M-x cider-jack-in`.  Wait for the
   REPL pane to appear.  Write your epic hack and bask in nerdvana.
