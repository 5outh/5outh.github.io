---
title: Tips for Generative Infrastructure and Tooling
author: Ben Kovach
date: 2018-10-13
tags:
  - "Generative Art"
  - Essays
---

Like with any programming project, certain aspects of generating artwork can
become repetitive or difficult over time. I want to discuss some
infrastructure/tooling related utilities that I've built around my art
generation process to make it easier to work with. This post is language-agnostic
in the sense that these ideas should be portable between different stacks.
Example code is presented in Haskell and zsh, but I aim to explain these
concepts well enough that the code doesn't matter all that much.

### Rendering new images on any source change

When I make a change in a file, I expect to see the result of that change as soon
as possible, without doing anything extra. That is a philosophy I stand by in all
of my programming work, for the most part:

- Code should re-compile when it changes
- Any relevant tests should re-run when code changes (if I am currently testing)
- A new image should be rendered when sketch source code changes

One language-agnostic tool for achieving this file-watching capability is
[entr](http://www.entrproject.org/). There are other file watchers out there,
but this one has been reliable for me. Given a program `generate-art` that
generates a new image, it can be used like this:

```sh
$ ls <source files> | entr generate-art
```

`<source files>` for me is `**/*.hs` in `zsh` - all [[Haskell]] files in the
current directory. When any of those files are changed, a new image is
re-rendered.

### Organizing images

...but, we don't just want one image. It's much better to have a snapshot of
**every single image that has ever been rendered** so we can pick and choose our
favorites and look back at how far we've come. We must organize.

My image organization technique is super simple. I store images in the following
directory heirarchy: `images/<name>/<seed>.png`, where

- `<name>` is the name of the piece I am currently working on. This is often
  something nebulous like `sketch4` while I'm messing around. Once a process
  starts to take shape, I change the name to something more concrete.
- `<seed>` is a UNIX Timestamp. When developing sketches, the current UNIX
  Timestamp is used as a seed, so a natural linear order is established within
  each folder.

Additionally, a single file `latest.png` is created and overwritten on save in
each folder. This allows me to leave `latest.png` open in an (auto-reloading)
image viewer, and see any changes automatically.

### Controlling variables

My art generation program has a thin CLI wrapper which I use to control certain
variables. Here's the help text as an overview:

```shell
$ drawing-hs --help
drawing-hs

Usage: drawing-hs [--seed SEED] [--scale SCALE] [--width WIDTH]
                  [--height HEIGHT] [--times TIMES] [--name NAME]
                  [-r|--render-progress] [-v|--render-video]
                  [--metadata METADATA]
  Generate art

Available options:
  -h,--help                Show this help text
```

This is tremendously useful and not particularly difficult behavior to achieve.
Here are some example scenarios that have made this absolutely invaluable:

<br />

I am generating a process that is kind of chaotic, but I like some of the
outputs a lot. I turn on the flag `--times=100` to render 100 random images
instead of just one.

<hr width="11%"/>
<br />

image size. I turn down the flag `--scale=10` to `--scale=5` to speed up the
rendering until I am happier with the process.

<hr width="11%"/>
<br />

I have generated hundreds of images and have picked out one that I like a lot. I
can set the seed using the `--seed` flag, along with a large scale using the `--scale`
flag to generate the large version for printing.

<hr width="11%"/>
<br />

I have found a particular seed I like a lot, but don't really like the color
palette I was using. I can set the `--seed` flag to lock the seed and tweak
non-random components of the program (in this instance, the color palette).

<hr width="11%"/>
<br />

I have decided that my `sketch` is good enough to be named. I pass in the `--name`
flag to start saving my files in a new location for polishing and better
organization.

<hr width="11%"/>

Extremely useful stuff. We'll talk about `--render-progress` and
`--render-video` later.

### Recovering lost sketches

Here's a short story. One day, I was iterating on a process and posted a work in
progress image to twitter. [Tyler Hobbs](http://www.tylerlhobbs.com) expressed
interest in a print swap with that work-in-progress image. Hell yeah, I thought!
Sounds awesome.

Unfortunately, I had iterated on that particular sketch a bit after posting. The
original image I had posted was not large enough to print, and I had no way of
pairing up the seed used to generate the image with the code that actually
generated it. I was able to recover it mostly, but the colors were not quite the
same as the image I posted on twitter, and it took a ton of effort to even
partially recover it. This really bothered me. Ultimately, we still swapped
prints, which was a wonderful experience. But I knew I had to fix the problem of
unrecoverable works in progress.

<center class="n/a">
  <blockquote class="twitter-tweet" data-lang="en"><p lang="und" dir="ltr"><a href="https://twitter.com/hashtag/generative?src=hash&amp;ref_src=twsrc%5Etfw">#generative</a> <a href="https://t.co/P80WvehjcW">pic.twitter.com/P80WvehjcW</a></p>&mdash; Ben (@BendotK) <a href="https://twitter.com/BendotK/status/984413805617795072?ref_src=twsrc%5Etfw">April 12, 2018</a></blockquote>
  <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>
</center>

Here's what I did:

- Create a completely new git repository in a separate location. I call it
  `sketch`.
- Whenever the program runs, copy the source code from the "real" drawing
  project to the `sketch` folder.
- Automatically create a git commit with the name of the `seed` in it.

This is implemented as a hook that runs after each image is generated. The
script `save_sketch` looks like this:

```bash
#! /bin/zsh

# Copy all source files to the sketch directory, overwriting anything that's
# already there
cp -R src/ ../sketch
cd ../sketch

# Create a git commit with the provided message, allowing empty commits in case
# no code was actually changed (allowing the reference of multiple sketches from
# the same code, with different seeds)
git add . -A
git commit -m "$1" --allow-empty
```

And in my code, I have this little function that runs as a part of the image
generation process:

```haskell
saveSketch :: Generate ()
saveSketch = do
  seed <- asks worldSeed
  liftIO $ do
    (code, out, _) <- readProcessWithExitCode
      "./scripts/save_sketch.sh"
      ["Automated publish; Seed: " <> show seed]
      ""
    putStrLn $ show code <> " // " <> out
```

For readers not fluent in [[Haskell]], this basically means "get the current seed
and call `./scripts.save_sketch.sh` with a commit message containing it".

Now, recovering sketches is just a matter of reaching into that repository and
copying it to the working directory:

```zsh
#! /bin/zsh

# Make sure we want to override the current source tree, since this is a
# destructive operation
read -q "RELOAD?Are you sure you want to restore history to ($1)? " -n 1 -r
echo

if [[ $RELOAD =~ ^[Yy]$ ]]
then
  cd ../sketch

  # Check out the git hash with the matching seed in "sketch" repository,
  # then copy it over to the "real" repository destructively. Afterwards,
  # switch back to the `master` branch of the "sketch" repository.
  COMMIT_HASH=$(git --no-pager log --grep "$1" --pretty=format:"%h")
  git checkout $COMMIT_HASH

  echo "Overwriting drawing-hs source..."
  cp -R src ../drawing-hs/src/

  echo "Resetting to master branch..."
  git checkout master
else
  echo "Aborting due to negative confirmation."
fi
```

This has not only been useful for situations like the print swap, but also for
iterative work. Sometimes, I go too far in one direction, look back and realize I
liked a previous iteration better. These scripts allow me to reset my progress back to
any previous state, which has the additional psychological side-effect of pushing
my work forward without fear of loss.

### Rendering video

My graphics framework doesn't allow native creation of videos. Instead, I render
a bunch of intermediate images with a specified format, then pass
those into `ffmpeg` to create an mp4. To do this, a few things are needed:

- A global counter, to track the frame number that is currently being rendered
- A way of formatting ints with a specific amount of padding (I use 8 digits)
- ffmpeg, of course

I have a utility function called `renderProgress` that renders a single png with
the surface that is currently being drawn. I can litter my program with
calls to `renderProgress` - often in loops - to render a given frame with the
appropriate index. This stores images in the folder
`./images/<name>/progress/<seed>/<index>.png`, where `<index>` is a frame index,
padded with 0s to 8 digits. For example:

```
$ ls -l images/example_sketch/progress/1539455256091/
total 72464
-rw-r--r-- 1 bendotk bendotk   5386 Oct 13 14:27 00000000.png
-rw-r--r-- 1 bendotk bendotk  10806 Oct 13 14:27 00000001.png
-rw-r--r-- 1 bendotk bendotk  15926 Oct 13 14:27 00000002.png
-rw-r--r-- 1 bendotk bendotk  21065 Oct 13 14:27 00000003.png
-rw-r--r-- 1 bendotk bendotk  25998 Oct 13 14:27 00000004.png
-rw-r--r-- 1 bendotk bendotk  30582 Oct 13 14:27 00000005.png
-rw-r--r-- 1 bendotk bendotk  35103 Oct 13 14:27 00000006.png
-rw-r--r-- 1 bendotk bendotk  36306 Oct 13 14:27 00000007.png
-rw-r--r-- 1 bendotk bendotk  34001 Oct 13 14:27 00000008.png
-rw-r--r-- 1 bendotk bendotk  34423 Oct 13 14:27 00000009.png
-rw-r--r-- 1 bendotk bendotk  35280 Oct 13 14:27 00000010.png
-rw-r--r-- 1 bendotk bendotk  37135 Oct 13 14:27 00000011.png
# ... and so on
```

Progress is only rendered if the `--render-progress` CLI flag is turned on;
this allows me to conditionally turn off intermediate rendering (which is slow)
without having to manually remove a bunch of function calls from my program.

In order to turn these frames into a video, I use a little wrapper around
`ffmpeg`:

```bash
#! /bin/zsh

# Arguments:
# $1: name of the sketch
# $2: seed of the sketch
# $3: desired frame rate

# Location of the eventual video file
VIDEO_FILE="./images/$1/progress/$2/progress.mp4"

# Use ffmpeg to create a video from the images in "images/$1/progress/$2/"
# in sequential order
ffmpeg -y -r $3 -f image2 -s 640x640 \
  -i images/$1/progress/$2/%08d.png -b:v 1024 \
  -vcodec libx264 -crf 25 -pix_fmt yuv420p $VIDEO_FILE

# Remove intermediate frames after video has been rendered
rm ./images/$1/progress/$2/*.png

# Open the video file after it has completed rendering.
xdg-open $VIDEO_FILE
```

This script is called with the appropriate arguments from within the image
rendering program if the `--render-video` option is supplied. I use a hard-coded
frame rate of 30 frames per second within the application, but occasionally I
will override that, so it is useful to retain as a command line flag. `ffmpeg`
is largely a mystery to me, so a lot of these options have been picked up after
troubleshooting and completely forgotten about after I got it working.

### Wrapping up

These things have saved me a lot of time and pain with a bit of fiddling around.
Feel free to [ping me on twitter](https://twitter.com/BendotK) if you have any
questions or comments. 🍻
