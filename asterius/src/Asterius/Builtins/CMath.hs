{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE HexFloatLiterals #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Asterius.Builtins.CMath
  ( cmathCBits,
  )
where

import Asterius.EDSL
import Asterius.Types

-- | Here contains implementation of certain functions in math.h of libc. They
-- are written in the expression IR directly without using our EDSL, and are not
-- meant to be understood by humans. For the curious souls, here's the
-- description of how they are written:
--
-- 1. Compile latest wasi-sdk to obtain the musl libc archives
-- 2. Extract the relevant object file from the archives, e.g. scalbn.o
-- 3. Use wasm2wat from wabt to disassemble the object file to human-readable
--    S-expression syntax
-- 4. Eyeball the disassembled function body, ensure it's "self-contained":
--    doesn't call into other libc functions, and doesn't touch the imported
--    linear memory
-- 5. Write the expression IR by hand, copying the disassembled code line by
--    line. Do not look back into the original musl C source, since the
--    amazement brought by clang/llvm optimizations is a distraction and slows
--    down this step
-- 6. Eyeball the written IR and disassembled code listed in two columns,
--    fingers crossed for no mistakes
-- 7. Include the code into the builtin module, run examples to see if anything
--    goes wrong, and run the ghc-testsuite to see if there's noticable
--    regression
--
-- This is a necessary evil before we have a proper story of clang integration.
cmathCBits :: AsteriusModule
cmathCBits = scalbn

-- | Implementation of the scalbn function in libc, used by __word_encodeDouble
-- in StgPrimFloats
scalbn :: AsteriusModule
scalbn =
  mempty
    { functionMap =
        [ ( "scalbn",
            Function
              { functionType =
                  FunctionType
                    { paramTypes = [F64, I32],
                      returnTypes = [F64]
                    },
                varTypes = [I32],
                body =
                  Block
                    { name = "",
                      bodys =
                        [ Block
                            { name = "1",
                              bodys =
                                [ Block
                                    { name = "2",
                                      bodys =
                                        [ Break
                                            { name = "2",
                                              breakCondition =
                                                Just $
                                                  GetLocal
                                                    { index = 1,
                                                      valueType = I32
                                                    }
                                                    `ltSInt32` ConstI32 1024
                                            },
                                          SetLocal
                                            { index = 0,
                                              value =
                                                GetLocal
                                                  { index = 0,
                                                    valueType = F64
                                                  }
                                                  `mulFloat64` ConstF64 0x1p+1023
                                            },
                                          Block
                                            { name = "3",
                                              bodys =
                                                [ Break
                                                    { name = "3",
                                                      breakCondition =
                                                        Just $
                                                          TeeLocal
                                                            { index = 2,
                                                              value =
                                                                GetLocal
                                                                  { index = 1,
                                                                    valueType =
                                                                      I32
                                                                  }
                                                                  `addInt32` ConstI32 (-1023),
                                                              valueType = I32
                                                            }
                                                            `geSInt32` ConstI32 1024
                                                    },
                                                  SetLocal
                                                    { index = 1,
                                                      value =
                                                        GetLocal
                                                          { index = 2,
                                                            valueType = I32
                                                          }
                                                    },
                                                  Break {name = "1", breakCondition = Nothing}
                                                ],
                                              blockReturnTypes = []
                                            },
                                          SetLocal
                                            { index = 0,
                                              value =
                                                GetLocal
                                                  { index = 0,
                                                    valueType = F64
                                                  }
                                                  `mulFloat64` ConstF64 0x1p+1023
                                            },
                                          SetLocal
                                            { index = 1,
                                              value =
                                                If
                                                  { condition =
                                                      GetLocal
                                                        { index = 1,
                                                          valueType =
                                                            I32
                                                        }
                                                        `ltSInt32` ConstI32 3069,
                                                    ifTrue =
                                                      GetLocal
                                                        { index = 1,
                                                          valueType = I32
                                                        },
                                                    ifFalse = Just $ ConstI32 3069
                                                  }
                                                  `addInt32` ConstI32 (-2046)
                                            },
                                          Break {name = "1", breakCondition = Nothing}
                                        ],
                                      blockReturnTypes = []
                                    },
                                  Break
                                    { name = "1",
                                      breakCondition =
                                        Just $
                                          GetLocal
                                            { index = 1,
                                              valueType = I32
                                            }
                                            `gtSInt32` ConstI32 (-1023)
                                    },
                                  SetLocal
                                    { index = 0,
                                      value =
                                        GetLocal
                                          { index = 0,
                                            valueType = F64
                                          }
                                          `mulFloat64` ConstF64 0x1p-969
                                    },
                                  Block
                                    { name = "2_",
                                      bodys =
                                        [ Break
                                            { name = "2_",
                                              breakCondition =
                                                Just $
                                                  TeeLocal
                                                    { index = 2,
                                                      value =
                                                        GetLocal
                                                          { index = 1,
                                                            valueType =
                                                              I32
                                                          }
                                                          `addInt32` ConstI32 969,
                                                      valueType = I32
                                                    }
                                                    `leSInt32` ConstI32 (-1023)
                                            },
                                          SetLocal
                                            { index = 1,
                                              value = GetLocal {index = 2, valueType = I32}
                                            },
                                          Break {name = "1", breakCondition = Nothing}
                                        ],
                                      blockReturnTypes = []
                                    },
                                  SetLocal
                                    { index = 0,
                                      value =
                                        GetLocal
                                          { index = 0,
                                            valueType = F64
                                          }
                                          `mulFloat64` ConstF64 0x1p-969
                                    },
                                  SetLocal
                                    { index = 1,
                                      value =
                                        If
                                          { condition =
                                              GetLocal
                                                { index = 1,
                                                  valueType = I32
                                                }
                                                `gtSInt32` ConstI32 (-2960),
                                            ifTrue =
                                              GetLocal
                                                { index = 1,
                                                  valueType = I32
                                                },
                                            ifFalse = Just $ ConstI32 (-2960)
                                          }
                                          `addInt32` ConstI32 1938
                                    }
                                ],
                              blockReturnTypes = []
                            },
                          GetLocal {index = 0, valueType = F64}
                            `mulFloat64` reinterpretInt64
                              ( extendUInt32
                                  ( GetLocal
                                      { index = 1,
                                        valueType = I32
                                      }
                                      `addInt32` ConstI32 1023
                                  )
                                  `shlInt64` ConstI64 52
                              )
                        ],
                      blockReturnTypes = [F64]
                    }
              }
          )
        ]
    }
