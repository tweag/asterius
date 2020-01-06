{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE HexFloatLiterals #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Asterius.Builtins.CMath
  ( cmathCBits,
  )
where

import Asterius.Types

cmathCBits :: AsteriusModule
cmathCBits = scalbn

scalbn :: AsteriusModule
scalbn =
  mempty
    { functionMap =
        [ ( "scalbn",
            Function
              { functionType = FunctionType
                  { paramTypes = [F64, I32],
                    returnTypes = [F64]
                  },
                varTypes = [I32],
                body = Block
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
                                            breakCondition = Just Binary
                                              { binaryOp = LtSInt32,
                                                operand0 = GetLocal
                                                  { index = 1,
                                                    valueType = I32
                                                  },
                                                operand1 = ConstI32 1024
                                              }
                                          },
                                        SetLocal
                                          { index = 0,
                                            value = Binary
                                              { binaryOp = MulFloat64,
                                                operand0 = GetLocal
                                                  { index = 0,
                                                    valueType = F64
                                                  },
                                                operand1 = ConstF64 0x1p+1023
                                              }
                                          },
                                        Block
                                          { name = "3",
                                            bodys =
                                              [ Break
                                                  { name = "3",
                                                    breakCondition = Just Binary
                                                      { binaryOp = GeSInt32,
                                                        operand0 = TeeLocal
                                                          { index = 2,
                                                            value = Binary
                                                              { binaryOp = AddInt32,
                                                                operand0 = GetLocal
                                                                  { index = 1,
                                                                    valueType =
                                                                      I32
                                                                  },
                                                                operand1 = ConstI32 (-1023)
                                                              },
                                                            valueType = I32
                                                          },
                                                        operand1 = ConstI32 1024
                                                      }
                                                  },
                                                SetLocal
                                                  { index = 1,
                                                    value = GetLocal
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
                                            value = Binary
                                              { binaryOp = MulFloat64,
                                                operand0 = GetLocal
                                                  { index = 0,
                                                    valueType = F64
                                                  },
                                                operand1 = ConstF64 0x1p+1023
                                              }
                                          },
                                        SetLocal
                                          { index = 1,
                                            value = Binary
                                              { binaryOp = AddInt32,
                                                operand0 = If
                                                  { condition = Binary
                                                      { binaryOp = LtSInt32,
                                                        operand0 = GetLocal
                                                          { index = 1,
                                                            valueType =
                                                              I32
                                                          },
                                                        operand1 = ConstI32 3069
                                                      },
                                                    ifTrue = GetLocal
                                                      { index = 1,
                                                        valueType = I32
                                                      },
                                                    ifFalse = Just $ ConstI32 3069
                                                  },
                                                operand1 = ConstI32 (-2046)
                                              }
                                          },
                                        Break {name = "1", breakCondition = Nothing}
                                      ],
                                    blockReturnTypes = []
                                  },
                                Break
                                  { name = "1",
                                    breakCondition = Just Binary
                                      { binaryOp = GtSInt32,
                                        operand0 = GetLocal
                                          { index = 1,
                                            valueType = I32
                                          },
                                        operand1 = ConstI32 (-1023)
                                      }
                                  },
                                SetLocal
                                  { index = 0,
                                    value = Binary
                                      { binaryOp = MulFloat64,
                                        operand0 = GetLocal
                                          { index = 0,
                                            valueType = F64
                                          },
                                        operand1 = ConstF64 0x1p-969
                                      }
                                  },
                                Block
                                  { name = "2_",
                                    bodys =
                                      [ Break
                                          { name = "2_",
                                            breakCondition = Just Binary
                                              { binaryOp = LeSInt32,
                                                operand0 = TeeLocal
                                                  { index = 2,
                                                    value = Binary
                                                      { binaryOp = AddInt32,
                                                        operand0 = GetLocal
                                                          { index = 1,
                                                            valueType =
                                                              I32
                                                          },
                                                        operand1 = ConstI32 969
                                                      },
                                                    valueType = I32
                                                  },
                                                operand1 = ConstI32 (-1023)
                                              }
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
                                    value = Binary
                                      { binaryOp = MulFloat64,
                                        operand0 = GetLocal
                                          { index = 0,
                                            valueType = F64
                                          },
                                        operand1 = ConstF64 0x1p-969
                                      }
                                  },
                                SetLocal
                                  { index = 1,
                                    value = Binary
                                      { binaryOp = AddInt32,
                                        operand0 = If
                                          { condition = Binary
                                              { binaryOp = GtSInt32,
                                                operand0 = GetLocal
                                                  { index = 1,
                                                    valueType = I32
                                                  },
                                                operand1 = ConstI32 (-2960)
                                              },
                                            ifTrue = GetLocal
                                              { index = 1,
                                                valueType = I32
                                              },
                                            ifFalse = Just $ ConstI32 (-2960)
                                          },
                                        operand1 = ConstI32 1938
                                      }
                                  }
                              ],
                            blockReturnTypes = []
                          },
                        Binary
                          { binaryOp = MulFloat64,
                            operand0 = GetLocal {index = 0, valueType = F64},
                            operand1 = Unary
                              { unaryOp = ReinterpretInt64,
                                operand0 = Binary
                                  { binaryOp = ShlInt64,
                                    operand0 = Unary
                                      { unaryOp = ExtendUInt32,
                                        operand0 = Binary
                                          { binaryOp = AddInt32,
                                            operand0 = GetLocal
                                              { index = 1,
                                                valueType = I32
                                              },
                                            operand1 = ConstI32 1023
                                          }
                                      },
                                    operand1 = ConstI64 52
                                  }
                              }
                          }
                      ],
                    blockReturnTypes = [F64]
                  }
              }
          )
        ]
    }
