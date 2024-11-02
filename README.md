# opendss-svn-mirror

➡️ The upstream/original copy is at https://sourceforge.net/p/electricdss/code/ ⬅️

---

⚠️ **WARNINGS** ⚠️

- **Do not rely on the commit hashes** since we might reorganize the repository someday. If you intend to use the code here, use the tags instead. There are initially no tags here, new and past recent releases will be tagged; feel free to fork the repository or request a tag if you need it. The projects on DSS-Extensions are free to use the commit hashes since we control them.
- **Avoid full clones!** This repository is large, close to 4 GB without a working tree. A full tree at recent revisions is close to 2.2 GB. Git's [sparse-checkout](https://github.blog/open-source/git/bring-your-monorepo-down-to-size-with-sparse-checkout/#cloning-in-sparse-mode) can help. For example, to clone only the last 100 commits and use a sparse checkout of a target folder, e.g. `trunk/Version8/Source`:

```shell
git clone --no-checkout --depth=100 -b svn https://github.com/dss-extensions/opendss-svn-mirror
cd opendss-svn-mirror
git sparse-checkout init --cone
git sparse-checkout set trunk/Version8/Source
git checkout svn
```

This is still a large download (e.g. 830 MB), but more reasonable. The working tree in this example is around 52 MB.

---

This is a (nearly) full **unofficial** mirror of the [OpenDSS](https://sourceforge.net/p/electricdss) SVN (Subversion repository), created using [git-svn](https://git-scm.com/docs/git-svn). As such, it preserves all the history of the public OpenDSS SVN repository, [starting on September 2008](https://github.com/dss-extensions/opendss-svn-mirror/tree/53ea452892d510ffbd609aae04ea327e282996fe).

The only files omitted were some of the temporary build artifacts (`*.rsm`, `*.dcu`, `*.obj`), and a single video file that was too large. There are more files/paths that could be removed in the future, e.g. `identcache` files.

GitHub has a limit of 100MB for files. Besides the mentioned video file, the current help file (`trunk/Version8/Doc/HelpNDocManual/OpenDSS_Documentation.hnd`) has grown very close to the limit. If it does grow being 100MB, we can stop tracking it here, or move the larger files to Git LFS, which supports 2GB files.

⛔ **This repository does not accept contributions, it's read only.** If you wish to contribute to the code of EPRI's OpenDSS engine or need support, please contact EPRI.

The alternative engine (AltDSS engine) from DSS-Extensions is currently hosted at https://github.com/dss-extensions/dss_capi/  
Our (DSS-Extensions) language bindings/interfaces/etc. are hosted on separate projects at https://github.com/dss-extensions/

## Why?

This repository should make it easier to consume the official OpenDSS binaries on the projects on DSS-Extensions.

Other copies from DSS-Extensions or floating around GitHub do not contain the whole history of the official SVN repository. For archival purposes, we decided to include everything since the first SVN revision. Only some corrupted commits are skipped.

## What's different from ...?

- [electricdss-tst](https://github.com/dss-extensions/electricdss-tst) is a filtered, light-weight copy of the test cases. We also add changes, minor fixes, and new test DSS scripts there.

- The branch [opendss-official-svn](https://github.com/dss-extensions/dss_capi/tree/opendss-official-svn) on the AltDSS engine/DSS C-API repository is a filtered copy that contains only the relevant Pascal source code, but no binaries.

## Where is it?

See the [`svn` branch](https://github.com/dss-extensions/opendss-svn-mirror/tree/svn). We will not make it the default branch to avoid wrong expectations from first impressions.

There might be some other *temporary* branches here as we test things.
