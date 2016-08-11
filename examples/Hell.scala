
object Hell {

  case class LLVMOpaqueContext()
  case class LLVMOpaqueModule()
  case class LLVMOpaqueType()
  case class LLVMOpaqueValue()
  case class LLVMOpaqueBasicBlock()
  case class LLVMOpaqueBuilder()
  case class LLVMBool()

  type LLVMContextRef = LLVMOpaqueContext
  type LLVMModuleRef = LLVMOpaqueModule
  type LLVMTypeRef = LLVMOpaqueType
  type LLVMValueRef = LLVMOpaqueValue
  type LLVMBasicBlockRef = LLVMOpaqueBasicBlock
  type LLVMBuilderRef = LLVMOpaqueBuilder


  def LLVMGetGlobalContext(): LLVMContextRef = ???

  def LLVMModuleCreateWithName(moduleID: String): LLVMModuleRef = ???

  def LLVMSetTarget(mod: LLVMModuleRef, triple: String) = ???

  def LLVMInt8Type(): LLVMTypeRef = ???

  def LLVMInt32Type(): LLVMTypeRef = ???

  def LLVMArrayType(ElementType: LLVMTypeRef, ElementCount: Int): LLVMTypeRef = ???

  def LLVMPointerType(ElementType: LLVMTypeRef, AddressSpace: Int): LLVMTypeRef = ???


  def LLVMFunctionType(returnType: LLVMTypeRef,
                       paramTypes: Array[LLVMTypeRef],
                       paramCount: Int,
                       isVarArg: LLVMBool): LLVMTypeRef = ???


  def arrayCreateLLVMTypeRef(size: Int): Array[LLVMTypeRef] = ???
  def arraySetLLVMTypeRef(data: Array[LLVMTypeRef], pos: Int, value: LLVMTypeRef) = ???


  def LLVMGetNamedFunction(module: LLVMModuleRef, name: String): LLVMValueRef = ???

  def LLVMAddFunction(module: LLVMModuleRef, name: String, FunctionTy: LLVMTypeRef): LLVMValueRef = ???

  def LLVMSetFunctionCallConv(fn: LLVMValueRef, cc: Int) = ???

  def notNullValueRef(v: LLVMValueRef): Boolean = ???

  def LLVMTrue(): LLVMBool = ???
  def LLVMFalse(): LLVMBool = ???

  def LLVMDumpModule(mod: LLVMModuleRef) = ???


  def LLVMAddGlobal(module: LLVMModuleRef, ty: LLVMTypeRef, name: String): LLVMValueRef = ???

  def LLVMSetAlignment(valRef: LLVMValueRef, bytes: Int) = ???


  def LLVMConstString(str: String, length: Int, notNullTerminate: LLVMBool): LLVMValueRef = ???

  def LLVMConstInt(intTy: LLVMTypeRef, n: Long, SignExtend: LLVMBool): LLVMValueRef = ???

  def LLVMCreateBuilder(): LLVMBuilderRef = ???


  def LLVMBuildInBoundsGEP(bb: LLVMBuilderRef, pointer: LLVMValueRef,
                           indices: Array[LLVMValueRef], numIndices: Int,
                           name: String): LLVMValueRef = ???


  def LLVMSetInitializer(globalVar: LLVMValueRef, constantVal: LLVMValueRef) = ???

  def LLVMAppendBasicBlock(fn: LLVMValueRef, name: String): LLVMBasicBlockRef = ???

  def LLVMPositionBuilderAtEnd(Builder: LLVMBuilderRef, Block: LLVMBasicBlockRef) = ???

  def LLVMBuildAlloca(b: LLVMBuilderRef, t: LLVMTypeRef, name: String): LLVMValueRef = ???

  def LLVMBuildStore(b: LLVMBuilderRef, v: LLVMValueRef, ptr: LLVMValueRef): LLVMValueRef = ???

  def LLVMBuildCall(g: LLVMBuilderRef, fn: LLVMValueRef, args: Array[LLVMValueRef],
                    numArgs: Int, name: String): LLVMValueRef = ???

  def LLVMSetTailCall(CallInst: LLVMValueRef, IsTailCall: LLVMBool) = ???

  def LLVMBuildRetVoid(b: LLVMBuilderRef): LLVMValueRef = ???

  def LLVMBuildRet(b: LLVMBuilderRef, v: LLVMValueRef): LLVMValueRef = ???

  def intToLong(i: Int): Long = ???


  // TODO add long type

  def main(args: Array[String]): Unit = {

    val LLVMCCallConv = 0

    // Module Construction
    val ctx = LLVMGetGlobalContext()
    val mod = LLVMModuleCreateWithName("debug.ll")
    LLVMSetTarget(mod, "x86_64-apple-macosx10.10.0")

    // Type Definitions
    val Int8Type = LLVMInt8Type()
    val Int32Type = LLVMInt32Type()
    val ArrayTy_0 = LLVMArrayType(Int8Type, 7)

    val PointerTy_1 = LLVMPointerType(ArrayTy_0, 0)

    val args_count13 = 0
    val FuncTy_2_args = new Array[LLVMTypeRef](args_count13)

    val FuncTy_2 = LLVMFunctionType(
        /*Result=*/Int32Type,
        /*Params=*/FuncTy_2_args,
        args_count13,
        /*isVarArg=*/LLVMFalse())

    val PointerTy_3 = LLVMPointerType(Int32Type, 0)
    val PointerTy_4 = LLVMPointerType(Int8Type, 0)

    val args_count6 = 1
    val FuncTy_6_args = new Array[LLVMTypeRef](args_count6)
    FuncTy_6_args(0) = PointerTy_4

    val FuncTy_6 = LLVMFunctionType(
      /*Result=*/Int32Type,
      /*Params=*/FuncTy_6_args,
      args_count6,
      /*isVarArg=*/LLVMTrue())

    val PointerTy_5 = LLVMPointerType(FuncTy_6, 0)

    // Function Declarations
    var func_main = LLVMGetNamedFunction(mod, "main")
    if (notNullValueRef(func_main)) {
      func_main = LLVMAddFunction(
        mod,
        /*Name=*/"main",
        /*Type=*/FuncTy_2
      )
      // (external, no body)
      LLVMSetFunctionCallConv(func_main, LLVMCCallConv)
    }

    var func_printf = LLVMGetNamedFunction(mod, "printf")
    if (notNullValueRef(func_printf)) {
      func_printf = LLVMAddFunction(
        mod,
        /*Name=*/"printf",
        /*Type=*/FuncTy_6
      )
      // (external, no body)
      LLVMSetFunctionCallConv(func_printf, LLVMCCallConv)
    }

    // Global Variable Declarations

    val gvar_array__str = LLVMAddGlobal(mod, ArrayTy_0, ".str")
    LLVMSetAlignment(gvar_array__str, 1)

    // Constant Definitions
    val const_array_7 = LLVMConstString("Hello", 7, LLVMTrue())

    val zero = LLVMConstInt(Int32Type, /*N=*/ intToLong(0), /*ext=*/ LLVMFalse())

    val const_ptr_10_indices = new Array[LLVMValueRef](2)
    const_ptr_10_indices(0) = zero
    const_ptr_10_indices(1) = zero

    val builder = LLVMCreateBuilder()

    val const_ptr_10 = LLVMBuildInBoundsGEP(builder, gvar_array__str, const_ptr_10_indices, 2, "NAME")

    // Global Variable Definitions
    LLVMSetInitializer(gvar_array__str, const_array_7)

    // Function Definitions

    // Function: main (func_main)

    val basicBlock = LLVMAppendBasicBlock(func_main, "entry")

    LLVMPositionBuilderAtEnd(builder, basicBlock)

    // Block  (label_11)
    val ptr_12 = LLVMBuildAlloca(builder, Int32Type, "ptr_12")
    val void_13 = LLVMBuildStore(builder, zero, ptr_12)
    val ttt = new Array[LLVMValueRef](1)
    ttt(0) = const_ptr_10
    val int32_14 = LLVMBuildCall(builder, func_printf, ttt, 1, "int32_14")


    //    LLVMSetFunctionCallConv(int32_14, LLVMCCallConv)

    LLVMSetTailCall(int32_14, LLVMFalse())

    LLVMBuildRet(builder, zero)


    LLVMDumpModule(mod)

    println("helloDotty!")

  }


}
