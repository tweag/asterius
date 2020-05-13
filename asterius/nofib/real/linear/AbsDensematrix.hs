

module AbsDensematrix(Block,Vec,bmult,bvecmult,vecbmult,vecdot,vecnorm,
                      vecouter,badd,bsub,vecadd,vecsub,bsize,vecsize,bneg,
                      bxpose,bident,vecneg,svecmult,mkblock,bswaprow,bswapcol,
                      bdroprow,bgetrow,bgetcol,bsubscript,vecsubscript,bupdate,
                      vecupdate,vechd,vectl,mergevecs,binverse,showblock,
                     showvec, mkvec,mkrvec,vecpart,update2,veclist,matlist)
       where


import Densematrix

type Block = Matrix
type Vec = Vector

bmult = mmult
bvecmult = matvecmult
vecbmult = vmmult
vecdot   = vdot

vecnorm = norm
vecouter = vouter

badd = madd
bsub = msub
vecadd = vadd
vecsub = vsub

bsize = msize
vecsize = vsize

bneg = mneg
bxpose = mxpose
bident = mident

vecneg = vneg
svecmult = svmult

mkblock = mkmat

bswaprow = swaprow
bswapcol = swapcol

bdroprow = droprow
bgetrow = getrow
bgetcol = getcol

bsubscript = subscript
vecsubscript = vsubscript

bupdate = update
vecupdate = vupdate

vechd = vhd
vectl = vtl

mergevecs = mergevectors

binverse = minverse

showblock = showmatrix
showvec = displayvector

