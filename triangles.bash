#!/usr/bin/env bash
set -euo pipefail

N=4000     # nodes
D=5        # ~edges per node

{
  # undirected edges: for each i, add D random links to j>i
  for ((i=0; i<N-1; i++)); do
    for ((k=0; k<D; k++)); do
      j=$(( (RANDOM % (N - i - 1)) + i + 1 ))
      echo "link($i,$j)."
    done
  done

  # total order for canonicalization (X<Y<Z) so each triangle appears once
  for ((i=0; i<N-1; i++)); do
    for ((j=i+1; j<N; j++)); do
      echo "lt($i,$j)."
    done
  done

  cat <<'DL'
// triangle in an undirected graph; link(X,Y) is symmetric by construction
triangle(X,Y,Z) :- link(X,Y), link(Y,Z), link(X,Z), lt(X,Y), lt(Y,Z).
DL
} | gigalog +RTS -N -A256M -s -RTS
