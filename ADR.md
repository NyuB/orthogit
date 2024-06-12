# Orthogit ADR

Orthogit is an experiment to make a git-like versioning system as **modular** and **generic** as possible, destructuring **orthogo**nal components.
It should be able to reproduce the 'Real Git' implementation as one possible combination of these modular components.

## Vocabulary
- 'Real Git' informal designation of the famous git program to distinguish from the components built in this project

## 12/06/2024
- Detached staging and branching from the core of git implementation
- Head behaviour (with auto update on commit etc) is still to detach too
- Use a generic data structure "Ref tree" for staging and interaction with git API
  + May not be the better structure
- Define Git tests as generic to ease other implementations

## 02/06/2024
### State of the project
We design Git as a versioning system for tree-like data structure with the following fundamentals types:
- 'Object', the actual data stored, corresponding to 'Real Git' files
- 'Tree', of which 'Object' are the leaves. This is the data structure 'stored' in our versioning system.
- 'Commit', snapshots of a Tree stored in the versioning system, associated with 'Metadata' and parents commits. Metadata correspond to Author/Commiter/Date... etc of a 'Real Git' commit.
- 'Id', unique identifiers to git objects. Sha1 in 'Real Git'.
- 'Labels', pointers to 'Id's that can be moved, corresponding to 'Real Git' refs

**NB**: Decision to treat parents of a commit as part of the fundamental type, and not as generic metadata.

- Actual storage of git types is modularized via the ObjectStorage notion, abstracting away what would be writes to .git/objects/...
  + **NB**: Identifying objects is handled by the object storage. i.e., we decided to retrieve Ids from a call to `id = store(obj)` instead of genrating the id then calling `store(id, obj)`
- Label storage is another modularized storage
- Branching, staging, committing have a first implementation
- 'Real Git' object parsing and serializing implemented, good milestone for the real git implementation

### Identified problems and ideas
- Staging can be done only in-memory and the Git implementation is too tightly coupled to its implementation(which is itself cumbersome)
  + Need to modularize Staging area as another module, with a cleaner interface
- Branching, staging, head moving could be modularized out of the main Git concept, making them optional components.
  + This would imply exposing only 'bare metal' writes (writeCommit, writeTree, writeBlob, ...) in Git.scala and lifting the 'commit()', 'add()' and the auto update behaviours in a trait extension

