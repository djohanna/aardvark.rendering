﻿namespace Aardvark.Rendering.GL

/// <summary>
/// represents all OpenGL functions used during execution.
/// note that resource creations, etc. are not included since they
/// don't need to be performed by the execution engine.
/// these InstructionCodes represent our abstract machine instructions.
/// </summary>
type InstructionCode = 
    | BindVertexArray          = 1
    | BindProgram              = 2
    | ActiveTexture            = 3
    | BindSampler              = 4
    | BindTexture              = 5
    | BindBufferBase           = 6
    | BindBufferRange          = 7
    | BindFramebuffer          = 8
    | Viewport                 = 9
    | Enable                   = 10
    | Disable                  = 11
    | DepthFunc                = 12
    | CullFace                 = 13
    | BlendFuncSeparate        = 14
    | BlendEquationSeparate    = 15
    | BlendColor               = 16
    | PolygonMode              = 17
    | StencilFuncSeparate      = 18
    | StencilOpSeparate        = 19
    | PatchParameter           = 20
    | DrawElements             = 21
    | DrawArrays               = 22
    | DrawElementsInstanced    = 23
    | DrawArraysInstanced      = 24
    | Clear                    = 25
    | BindImageTexture         = 26
    | ClearColor               = 27
    | ClearDepth               = 28
    | GetError                 = 29
    | BindBuffer               = 30
    | VertexAttribPointer      = 31
    | VertexAttribDivisor      = 32
    | EnableVertexAttribArray  = 33
    | DisableVertexAttribArray = 34

    | Uniform1fv               = 35
    | Uniform1iv               = 36
    | Uniform2fv               = 37
    | Uniform2iv               = 38
    | Uniform3fv               = 39
    | Uniform3iv               = 40
    | Uniform4fv               = 41
    | Uniform4iv               = 42
    | UniformMatrix2fv         = 43
    | UniformMatrix3fv         = 44
    | UniformMatrix4fv         = 45

/// <summary>
/// an instrution consists of an instruction-code and the corresponding arguments
/// which are internally held as object-array. 
/// Note that the primary constructor is private and instructions can only
/// be created using the static creator-functions.
/// </summary>
type Instruction private(code : InstructionCode, args : obj[]) =

    // auxillary functions for converting fractional values
    // to their integer-represenations for uniformity of the underlying
    // execution engine. 
    static let bits (f : float32) =
        System.BitConverter.ToInt32(System.BitConverter.GetBytes f, 0)

    static let dbits (f : float) =
        System.BitConverter.ToInt64(System.BitConverter.GetBytes f, 0)

    static let uniform (code : InstructionCode) (l : int) (c : int) (ptr : nativeint) =
        Instruction(code, [|l :> obj; c :> obj; ptr :> obj|])


    member x.Operation = code
    member x.Arguments = args

    override x.ToString() =
        sprintf "%A(%s)" code (args |> Array.map (sprintf "%A") |> String.concat ", ")


    static member BindVertexArray (vao : int) = Instruction(InstructionCode.BindVertexArray, [|vao :> obj|])
    static member BindProgram (program : int) = Instruction(InstructionCode.BindProgram, [|program :> obj|])
    static member ActiveTexture (unit : int) = Instruction(InstructionCode.ActiveTexture, [|unit :> obj|])
    static member BindSampler (unit : int) (sampler : int) = Instruction(InstructionCode.BindSampler, [|unit :> obj; sampler :> obj|])
    static member BindTexture (target : int) (texture : int) = Instruction(InstructionCode.BindTexture, [|target :> obj; texture :> obj|])
    static member BindBuffer (target : int) (buffer : int) = Instruction(InstructionCode.BindBuffer, [|target :> obj; buffer :> obj|])
    static member BindBufferBase (target : int) (index : int) (buffer : int) = Instruction(InstructionCode.BindBufferBase, [|target :> obj; index :> obj; buffer :> obj|])
    static member BindBufferRange (target : int) (index : int) (buffer : int) (offset : nativeint) (size : nativeint) = Instruction(InstructionCode.BindBufferRange, [|target :> obj; index :> obj; buffer :> obj; offset :> obj; size :> obj|])
    static member BindFramebuffer (target : int) (fbo : int) = Instruction(InstructionCode.BindFramebuffer, [|target :> obj; fbo :> obj|])
    static member Viewport (x : int) (y : int) (w : int) (h : int) = Instruction(InstructionCode.Viewport, [|x :> obj; y :> obj; w :> obj; h :> obj|])
    static member Enable (flag : int) = Instruction(InstructionCode.Enable, [|flag :> obj|])
    static member Disable (flag : int) = Instruction(InstructionCode.Disable, [|flag :> obj|])
    static member DepthFunc (func : int) = Instruction(InstructionCode.DepthFunc, [|func :> obj|])
    static member CullFace (face : int) = Instruction(InstructionCode.CullFace, [|face :> obj|])
    static member BlendFuncSeparate (srcRGB : int) (dstRGB : int) (srcAlpha : int) (dstAlpha : int) = Instruction(InstructionCode.BlendFuncSeparate, [|srcRGB :> obj; dstRGB :> obj; srcAlpha :> obj; dstAlpha :> obj|])
    static member BlendEquationSeparate (modeRGB : int) (modeAlpha : int) = Instruction(InstructionCode.BlendEquationSeparate, [|modeRGB :> obj; modeAlpha :> obj|])
    static member BlendColor (r : float32) (g : float32) (b : float32) (a : float32) = Instruction(InstructionCode.BlendColor, [|bits r :> obj; bits g :> obj; bits b :> obj; bits a :> obj|])
    static member PolygonMode (face : int) (mode : int) = Instruction(InstructionCode.PolygonMode, [|face :> obj; mode :> obj|])
    static member StencilFuncSeparate (face : int) (func : int) (ref : int) (mask : int) = Instruction(InstructionCode.StencilFuncSeparate, [|face :> obj; func :> obj; ref :> obj; mask :> obj|])
    static member StencilOpSeparate (face : int) (sfail : int) (dfail : int) (dpass : int) = Instruction(InstructionCode.StencilOpSeparate, [|face :> obj; sfail :> obj; dfail :> obj; dpass :> obj|])
    static member PatchParameter (pname : int) (value : int) = Instruction(InstructionCode.PatchParameter, [|pname :> obj; value :> obj|])
    static member DrawElements (mode : int) (count : int) (t : int) (indices : nativeint) = Instruction(InstructionCode.DrawElements, [|mode :> obj; count :> obj; t :> obj; indices :> obj|])
    static member DrawArrays (mode : int) (first : int) (count : int) = Instruction(InstructionCode.DrawArrays, [|mode :> obj; first :> obj; count :> obj|])
    static member DrawElementsInstanced (mode : int) (count : int) (t : int) (indices : nativeint) (primcount : int) = Instruction(InstructionCode.DrawElementsInstanced, [|mode :> obj; count :> obj; t :> obj; indices :> obj; primcount :> obj|])
    static member DrawArraysInstanced (mode : int) (first : int) (count : int) (primcount : int) = Instruction(InstructionCode.DrawArraysInstanced, [|mode :> obj; first :> obj; count :> obj; primcount :> obj|])
    static member Clear (mask : int) = Instruction(InstructionCode.Clear, [|mask :> obj|])
    static member ClearColor (r : float32) (g : float32) (b : float32) (a : float32) = Instruction(InstructionCode.ClearColor, [|bits r :> obj; bits g :> obj; bits b :> obj; bits a :> obj|])
    static member ClearDepth (depth : float) = Instruction(InstructionCode.ClearDepth, [|dbits depth :> obj|])
    static member VertexAttribPointer (index : int) (size : int) (t : int) (normalized : bool) (stride : int) (ptr : nativeint) = Instruction(InstructionCode.VertexAttribPointer, [|index :> obj; size :> obj; t :> obj; (if normalized then 1 else 0) :> obj; stride :> obj; ptr :> obj|])
    static member VertexAttribDivisor (index : int) (divisor : int) = Instruction(InstructionCode.VertexAttribDivisor, [|index :> obj; divisor :> obj|])
    static member EnableVertexAttribArray (index : int)  = Instruction(InstructionCode.EnableVertexAttribArray, [|index :> obj|])
    static member DisableVertexAttribArray (index : int)  = Instruction(InstructionCode.DisableVertexAttribArray, [|index :> obj|])



    static member Uniform1fv (l : int) (c : int) (p : nativeint)  = uniform InstructionCode.Uniform1fv l c p
    static member Uniform1iv (l : int) (c : int) (p : nativeint)  = uniform InstructionCode.Uniform1iv l c p
    static member Uniform2fv (l : int) (c : int) (p : nativeint)  = uniform InstructionCode.Uniform2fv l c p
    static member Uniform2iv (l : int) (c : int) (p : nativeint)  = uniform InstructionCode.Uniform2iv l c p
    static member Uniform3fv (l : int) (c : int) (p : nativeint)  = uniform InstructionCode.Uniform3fv l c p
    static member Uniform3iv (l : int) (c : int) (p : nativeint)  = uniform InstructionCode.Uniform3iv l c p
    static member Uniform4fv (l : int) (c : int) (p : nativeint)  = uniform InstructionCode.Uniform4fv l c p
    static member Uniform4iv (l : int) (c : int) (p : nativeint)  = uniform InstructionCode.Uniform4iv l c p


    static member UniformMatrix2fv (location : int) (count : int) (transpose : int) (ptr : nativeint)  = 
        Instruction(InstructionCode.UniformMatrix2fv, [|location :> obj; count :> obj; transpose :> obj; ptr :> obj|])

    static member UniformMatrix3fv (location : int) (count : int) (transpose : int) (ptr : nativeint)  = 
        Instruction(InstructionCode.UniformMatrix3fv, [|location :> obj; count :> obj; transpose :> obj; ptr :> obj|])

    static member UniformMatrix4fv (location : int) (count : int) (transpose : int) (ptr : nativeint)  = 
        Instruction(InstructionCode.UniformMatrix4fv, [|location :> obj; count :> obj; transpose :> obj; ptr :> obj|])