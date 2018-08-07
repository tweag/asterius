/*
 * Common macros for containers
 */

#ifndef HASKELL_CONTAINERS_H
#define HASKELL_CONTAINERS_H

/*
 * On GHC, include MachDeps.h to get WORD_SIZE_IN_BITS macro.
 */
#ifdef __GLASGOW_HASKELL__
#include "MachDeps.h"
#endif

/*
 * Define INSTANCE_TYPEABLE[0-2]
 */
#if __GLASGOW_HASKELL__ >= 707
#define INSTANCE_TYPEABLE0(tycon) deriving instance Typeable tycon
#define INSTANCE_TYPEABLE1(tycon) deriving instance Typeable tycon
#define INSTANCE_TYPEABLE2(tycon) deriving instance Typeable tycon
#elif defined(__GLASGOW_HASKELL__)
#define INSTANCE_TYPEABLE0(tycon) deriving instance Typeable tycon
#define INSTANCE_TYPEABLE1(tycon) deriving instance Typeable1 tycon
#define INSTANCE_TYPEABLE2(tycon) deriving instance Typeable2 tycon
#else
#define INSTANCE_TYPEABLE0(tycon)
#define INSTANCE_TYPEABLE1(tycon)
#define INSTANCE_TYPEABLE2(tycon)
#endif

#if __GLASGOW_HASKELL__ >= 800
#define DEFINE_PATTERN_SYNONYMS 1
#endif

#ifdef __GLASGOW_HASKELL__
# define USE_ST_MONAD 1
# define USE_UNBOXED_ARRAYS 1
#endif

#endif
