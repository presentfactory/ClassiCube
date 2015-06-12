#region License
//
// The Open Toolkit Library License
//
// Copyright (c) 2006 - 2009 the Open Toolkit library.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights to
// use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
// the Software, and to permit persons to whom the Software is furnished to do
// so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
// HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
// WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
// OTHER DEALINGS IN THE SOFTWARE.
//
#endregion

namespace OpenTK.Graphics.OpenGL
{
	using System;
	using System.Text;
	using System.Runtime.InteropServices;
	#pragma warning disable 3019
	#pragma warning disable 1591
	#pragma warning disable 1572
	#pragma warning disable 1573

	partial class GL
	{

		public static partial class Arb
		{
			[AutoGenerated(Category = "ArbVertexBufferObject", Version = "1.2", EntryPoint = "glBindBufferARB")]
			public static void BindBuffer(BufferTargetArb target, Int32 buffer) {
				Delegates.glBindBufferARB(target, (UInt32)buffer);
			}

			[AutoGenerated(Category = "ArbVertexBufferObject", Version = "1.2", EntryPoint = "glBufferDataARB")]
			public static void BufferData(BufferTargetArb target, IntPtr size, IntPtr data, BufferUsageArb usage) {
				Delegates.glBufferDataARB(target, size, data, usage);
			}

			[AutoGenerated(Category = "ArbVertexBufferObject", Version = "1.2", EntryPoint = "glBufferDataARB")]
			public static void BufferData<T2>(BufferTargetArb target, IntPtr size, [InAttribute, OutAttribute] T2[] data, BufferUsageArb usage)
				where T2 : struct {
				GCHandle data_ptr = GCHandle.Alloc(data, GCHandleType.Pinned);
				try {
					Delegates.glBufferDataARB(target, size, data_ptr.AddrOfPinnedObject(), usage);
				} finally {
					data_ptr.Free();
				}
			}

			[AutoGenerated(Category = "ArbVertexBufferObject", Version = "1.2", EntryPoint = "glBufferDataARB")]
			public static void BufferData<T2>(BufferTargetArb target, IntPtr size, [InAttribute, OutAttribute] ref T2 data, BufferUsageArb usage)
				where T2 : struct {
				GCHandle data_ptr = GCHandle.Alloc(data, GCHandleType.Pinned);
				try {
					Delegates.glBufferDataARB(target, size, data_ptr.AddrOfPinnedObject(), usage);
					data = (T2)data_ptr.Target;
				} finally {
					data_ptr.Free();
				}
			}

			[AutoGenerated(Category = "ArbVertexBufferObject", Version = "1.2", EntryPoint = "glBufferSubDataARB")]
			public static void BufferSubData(BufferTargetArb target, IntPtr offset, IntPtr size, IntPtr data) {
				Delegates.glBufferSubDataARB(target, offset, size, data);
			}

			[AutoGenerated(Category = "ArbVertexBufferObject", Version = "1.2", EntryPoint = "glBufferSubDataARB")]
			public static void BufferSubData<T3>(BufferTargetArb target, IntPtr offset, IntPtr size, [InAttribute, OutAttribute] T3[] data)
				where T3 : struct {
				GCHandle data_ptr = GCHandle.Alloc(data, GCHandleType.Pinned);
				try {
					Delegates.glBufferSubDataARB(target, offset, size, data_ptr.AddrOfPinnedObject());
				} finally {
					data_ptr.Free();
				}
			}

			[AutoGenerated(Category = "ArbVertexBufferObject", Version = "1.2", EntryPoint = "glBufferSubDataARB")]
			public static void BufferSubData<T3>(BufferTargetArb target, IntPtr offset, IntPtr size, [InAttribute, OutAttribute] ref T3 data)
				where T3 : struct
			{
				GCHandle data_ptr = GCHandle.Alloc(data, GCHandleType.Pinned);
				try {
					Delegates.glBufferSubDataARB(target, offset, size, data_ptr.AddrOfPinnedObject());
					data = (T3)data_ptr.Target;
				} finally {
					data_ptr.Free();
				}
			}
			
			[System.CLSCompliant(false)]
			[AutoGenerated(Category = "ArbVertexBufferObject", Version = "1.2", EntryPoint = "glDeleteBuffersARB")]
			public static unsafe void DeleteBuffers(Int32 n, Int32* buffers) {
				Delegates.glDeleteBuffersARB(n, (UInt32*)buffers);
			}
			
			[System.CLSCompliant(false)]
			[AutoGenerated(Category = "ArbVertexBufferObject", Version = "1.2", EntryPoint = "glGenBuffersARB")]
			public static unsafe void GenBuffers(Int32 n, [OutAttribute] Int32* buffers) {
				Delegates.glGenBuffersARB(n, (UInt32*)buffers);
			}
			
			[AutoGenerated(Category = "ArbVertexBufferObject", Version = "1.2", EntryPoint = "glIsBufferARB")]
			public static bool IsBuffer(Int32 buffer) {
				return Delegates.glIsBufferARB((UInt32)buffer);
			}
		}

		[AutoGenerated(Category = "Version10Deprecated", Version = "1.0", EntryPoint = "glAlphaFunc")]
		public static void AlphaFunc(AlphaFunction func, Single @ref) {
			Delegates.glAlphaFunc(func, @ref);
		}
		
		[AutoGenerated(Category = "Version15", Version = "1.5", EntryPoint = "glBindBuffer")]
		public static void BindBuffer(BufferTarget target, Int32 buffer) {
			Delegates.glBindBuffer(target, (UInt32)buffer);
		}
		
		[AutoGenerated(Category = "Version11", Version = "1.1", EntryPoint = "glBindTexture")]
		public static void BindTexture(TextureTarget target, Int32 texture) {
			Delegates.glBindTexture(target, (UInt32)texture);
		}

		[AutoGenerated(Category = "Version10", Version = "1.0", EntryPoint = "glBlendFunc")]
		public static void BlendFunc(BlendingFactor sfactor, BlendingFactor dfactor) {
			Delegates.glBlendFunc(sfactor, dfactor);
		}

		[AutoGenerated(Category = "Version15", Version = "1.5", EntryPoint = "glBufferData")]
		public static void BufferData(BufferTarget target, IntPtr size, IntPtr data, BufferUsageHint usage) {
			Delegates.glBufferData(target, size, data, usage);
		}

		[AutoGenerated(Category = "Version15", Version = "1.5", EntryPoint = "glBufferData")]
		public static void BufferData<T2>(BufferTarget target, IntPtr size, [InAttribute, OutAttribute] T2[] data, BufferUsageHint usage)
			where T2 : struct {
			GCHandle data_ptr = GCHandle.Alloc(data, GCHandleType.Pinned);
			try {
				Delegates.glBufferData(target, size, data_ptr.AddrOfPinnedObject(), usage);
			} finally {
				data_ptr.Free();
			}
		}

		[AutoGenerated(Category = "Version15", Version = "1.5", EntryPoint = "glBufferData")]
		public static void BufferData<T2>(BufferTarget target, IntPtr size, [InAttribute, OutAttribute] ref T2 data, BufferUsageHint usage)
			where T2 : struct {
			GCHandle data_ptr = GCHandle.Alloc(data, GCHandleType.Pinned);
			try {
				Delegates.glBufferData(target, size, data_ptr.AddrOfPinnedObject(), usage);
				data = (T2)data_ptr.Target;
			} finally {
				data_ptr.Free();
			}
		}

		[AutoGenerated(Category = "Version15", Version = "1.5", EntryPoint = "glBufferSubData")]
		public static void BufferSubData(BufferTarget target, IntPtr offset, IntPtr size, IntPtr data) {
			Delegates.glBufferSubData(target, offset, size, data);
		}

		[AutoGenerated(Category = "Version15", Version = "1.5", EntryPoint = "glBufferSubData")]
		public static void BufferSubData<T3>(BufferTarget target, IntPtr offset, IntPtr size, [InAttribute, OutAttribute] T3[] data)
			where T3 : struct {
			GCHandle data_ptr = GCHandle.Alloc(data, GCHandleType.Pinned);
			try {
				Delegates.glBufferSubData(target, offset, size, data_ptr.AddrOfPinnedObject());
			} finally {
				data_ptr.Free();
			}
		}

		[AutoGenerated(Category = "Version15", Version = "1.5", EntryPoint = "glBufferSubData")]
		public static void BufferSubData<T3>(BufferTarget target, IntPtr offset, IntPtr size, [InAttribute, OutAttribute] ref T3 data)
			where T3 : struct {
			GCHandle data_ptr = GCHandle.Alloc(data, GCHandleType.Pinned);
			try {
				Delegates.glBufferSubData(target, offset, size, data_ptr.AddrOfPinnedObject());
				data = (T3)data_ptr.Target;
			} finally {
				data_ptr.Free();
			}
		}
		
		[AutoGenerated(Category = "Version10", Version = "1.0", EntryPoint = "glClear")]
		public static void Clear(ClearBufferMask mask) {
			Delegates.glClear(mask);
		}
		
		[AutoGenerated(Category = "Version10", Version = "1.0", EntryPoint = "glClearColor")]
		public static void ClearColor(Single red, Single green, Single blue, Single alpha) {
			Delegates.glClearColor(red, green, blue, alpha);
		}
		
		[AutoGenerated(Category = "Version10", Version = "1.0", EntryPoint = "glClearDepth")]
		public static void ClearDepth(Double depth) {
			Delegates.glClearDepth(depth);
		}
		
		[AutoGenerated(Category = "Version10", Version = "1.0", EntryPoint = "glColorMask")]
		public static void ColorMask(bool red, bool green, bool blue, bool alpha) {
			Delegates.glColorMask(red, green, blue, alpha);
		}
		
		[AutoGenerated(Category = "Version11Deprecated", Version = "1.1", EntryPoint = "glColorPointer")]
		public static void ColorPointer(Int32 size, ColorPointerType type, Int32 stride, IntPtr pointer) {
			Delegates.glColorPointer(size, type, stride, pointer);
		}
		
		[AutoGenerated(Category = "Version11", Version = "1.1", EntryPoint = "glCopyTexImage2D")]
		public static void CopyTexImage2D(TextureTarget target, Int32 level, PixelInternalFormat internalformat, Int32 x, Int32 y, Int32 width, Int32 height, Int32 border) {
			Delegates.glCopyTexImage2D(target, level, internalformat, x, y, width, height, border);
		}
		
		[AutoGenerated(Category = "Version11", Version = "1.1", EntryPoint = "glCopyTexSubImage2D")]
		public static void CopyTexSubImage2D(TextureTarget target, Int32 level, Int32 xoffset, Int32 yoffset, Int32 x, Int32 y, Int32 width, Int32 height) {
			Delegates.glCopyTexSubImage2D(target, level, xoffset, yoffset, x, y, width, height);
		}

		[AutoGenerated(Category = "Version10", Version = "1.0", EntryPoint = "glCullFace")]
		public static void CullFace(CullFaceMode mode) {
			Delegates.glCullFace(mode);
		}
		
		[System.CLSCompliant(false)]
		[AutoGenerated(Category = "Version15", Version = "1.5", EntryPoint = "glDeleteBuffers")]
		public static unsafe void DeleteBuffers(Int32 n, Int32* buffers) {
			Delegates.glDeleteBuffers(n, (UInt32*)buffers);
		}
		
		[System.CLSCompliant(false)]
		[AutoGenerated(Category = "Version11", Version = "1.1", EntryPoint = "glDeleteTextures")]
		public static unsafe void DeleteTextures(Int32 n, Int32* textures) {
			Delegates.glDeleteTextures(n, (UInt32*)textures);
		}
		
		[AutoGenerated(Category = "Version10", Version = "1.0", EntryPoint = "glDepthFunc")]
		public static void DepthFunc(DepthFunction func) {
			Delegates.glDepthFunc(func);
		}
		
		[AutoGenerated(Category = "Version10", Version = "1.0", EntryPoint = "glDepthMask")]
		public static void DepthMask(bool flag) {
			Delegates.glDepthMask(flag);
		}
		
		[AutoGenerated(Category = "Version10", Version = "1.0", EntryPoint = "glDepthRange")]
		public static void DepthRange(Double near, Double far) {
			Delegates.glDepthRange(near, far);
		}

		[AutoGenerated(Category = "Version10", Version = "1.0", EntryPoint = "glDisable")]
		public static void Disable(EnableCap cap) {
			Delegates.glDisable(cap);
		}

		[AutoGenerated(Category = "Version11Deprecated", Version = "1.1", EntryPoint = "glDisableClientState")]
		public static void DisableClientState(ArrayCap array) {
			Delegates.glDisableClientState(array);
		}
		
		[AutoGenerated(Category = "Version11", Version = "1.1", EntryPoint = "glDrawArrays")]
		public static void DrawArrays(BeginMode mode, Int32 first, Int32 count) {
			Delegates.glDrawArrays(mode, first, count);
		}
		
		[AutoGenerated(Category = "Version11", Version = "1.1", EntryPoint = "glDrawElements")]
		public static void DrawElements(BeginMode mode, Int32 count, DrawElementsType type, IntPtr indices) {
			Delegates.glDrawElements(mode, count, type, indices);
		}

		[AutoGenerated(Category = "Version10", Version = "1.0", EntryPoint = "glEnable")]
		public static void Enable(EnableCap cap) {
			Delegates.glEnable(cap);
		}

		[AutoGenerated(Category = "Version11Deprecated", Version = "1.1", EntryPoint = "glEnableClientState")]
		public static void EnableClientState(ArrayCap array) {
			Delegates.glEnableClientState(array);
		}
		
		[AutoGenerated(Category = "Version10", Version = "1.0", EntryPoint = "glFinish")]
		public static void Finish() {
			Delegates.glFinish();
		}
		
		[AutoGenerated(Category = "Version10", Version = "1.0", EntryPoint = "glFlush")]
		public static void Flush() {
			Delegates.glFlush();
		}

		[AutoGenerated(Category = "Version10Deprecated", Version = "1.0", EntryPoint = "glFogf")]
		public static void Fog(FogParameter pname, Single param) {
			Delegates.glFogf(pname, param);
		}

		[System.CLSCompliant(false)]
		[AutoGenerated(Category = "Version10Deprecated", Version = "1.0", EntryPoint = "glFogfv")]
		public static unsafe void Fog(FogParameter pname, Single* @params) {
			Delegates.glFogfv(pname, @params);
		}

		[AutoGenerated(Category = "Version10Deprecated", Version = "1.0", EntryPoint = "glFogi")]
		public static void Fog(FogParameter pname, Int32 param) {
			Delegates.glFogi(pname, param);
		}

		[System.CLSCompliant(false)]
		[AutoGenerated(Category = "Version10Deprecated", Version = "1.0", EntryPoint = "glFogiv")]
		public static unsafe void Fog(FogParameter pname, Int32* @params) {
			Delegates.glFogiv(pname, (Int32*)@params);
		}

		[AutoGenerated(Category = "Version10", Version = "1.0", EntryPoint = "glFrontFace")]
		public static void FrontFace(FrontFaceDirection mode) {
			Delegates.glFrontFace(mode);
		}
		
		[System.CLSCompliant(false)]
		[AutoGenerated(Category = "Version15", Version = "1.5", EntryPoint = "glGenBuffers")]
		public static unsafe void GenBuffers(Int32 n, [OutAttribute] Int32* buffers) {
			Delegates.glGenBuffers(n, (UInt32*)buffers);
		}
		
		[System.CLSCompliant(false)]
		[AutoGenerated(Category = "Version11", Version = "1.1", EntryPoint = "glGenTextures")]
		public static unsafe void GenTextures(Int32 n, [OutAttribute] Int32* textures) {
			Delegates.glGenTextures(n, (UInt32*)textures);
		}

		[AutoGenerated(Category = "Version10", Version = "1.0", EntryPoint = "glGetBooleanv")]
		public static unsafe void GetBoolean(GetPName pname, [OutAttribute] bool[] @params) {
			fixed (bool* @params_ptr = @params) {
				Delegates.glGetBooleanv(pname, (bool*)@params_ptr);
			}
		}

		[AutoGenerated(Category = "Version10", Version = "1.0", EntryPoint = "glGetBooleanv")]
		public static unsafe void GetBoolean(GetPName pname, [OutAttribute] out bool @params) {
			fixed (bool* @params_ptr = &@params) {
				Delegates.glGetBooleanv(pname, (bool*)@params_ptr);
				@params = *@params_ptr;
			}
		}

		[System.CLSCompliant(false)]
		[AutoGenerated(Category = "Version10", Version = "1.0", EntryPoint = "glGetBooleanv")]
		public static unsafe void GetBoolean(GetPName pname, [OutAttribute] bool* @params) {
			Delegates.glGetBooleanv(pname, (bool*)@params);
		}
		
		[AutoGenerated(Category = "Version10", Version = "1.0", EntryPoint = "glGetError")]
		public static ErrorCode GetError() {
			return Delegates.glGetError();
		}

		[AutoGenerated(Category = "Version10", Version = "1.0", EntryPoint = "glGetFloatv")]
		public static unsafe void GetFloat(GetPName pname, [OutAttribute] Single[] @params) {
			fixed (Single* @params_ptr = @params) {
				Delegates.glGetFloatv(pname, @params_ptr);
			}
		}

		[AutoGenerated(Category = "Version10", Version = "1.0", EntryPoint = "glGetFloatv")]
		public static unsafe void GetFloat(GetPName pname, [OutAttribute] out Single @params) {
			fixed (Single* @params_ptr = &@params) {
				Delegates.glGetFloatv(pname, @params_ptr);
				@params = *@params_ptr;
			}
		}

		[System.CLSCompliant(false)]
		[AutoGenerated(Category = "Version10", Version = "1.0", EntryPoint = "glGetFloatv")]
		public static unsafe void GetFloat(GetPName pname, [OutAttribute] Single* @params) {
			Delegates.glGetFloatv(pname, @params);
		}

		[AutoGenerated(Category = "Version10", Version = "1.0", EntryPoint = "glGetIntegerv")]
		public static unsafe void GetInteger(GetPName pname, [OutAttribute] Int32[] @params) {
			fixed (Int32* @params_ptr = @params) {
				Delegates.glGetIntegerv(pname, @params_ptr);
			}
		}

		[AutoGenerated(Category = "Version10", Version = "1.0", EntryPoint = "glGetIntegerv")]
		public static unsafe void GetInteger(GetPName pname, [OutAttribute] out Int32 @params) {
			fixed (Int32* @params_ptr = &@params) {
				Delegates.glGetIntegerv(pname, @params_ptr);
				@params = *@params_ptr;
			}
		}

		[System.CLSCompliant(false)]
		[AutoGenerated(Category = "Version10", Version = "1.0", EntryPoint = "glGetIntegerv")]
		public static unsafe void GetInteger(GetPName pname, [OutAttribute] Int32* @params) {
			Delegates.glGetIntegerv(pname, (Int32*)@params);
		}
		
		[AutoGenerated(Category = "Version10", Version = "1.0", EntryPoint = "glGetString")]
		public unsafe static System.String GetString(StringName name) {
			return new string((sbyte*)Delegates.glGetString(name));
		}

		[AutoGenerated(Category = "Version10", Version = "1.0", EntryPoint = "glGetTexImage")]
		public static void GetTexImage(TextureTarget target, Int32 level, PixelFormat format, PixelType type, [OutAttribute] IntPtr pixels) {
			Delegates.glGetTexImage(target, level, format, type, pixels);
		}
		
		[AutoGenerated(Category = "Version10", Version = "1.0", EntryPoint = "glHint")]
		public static void Hint(HintTarget target, HintMode mode) {
			Delegates.glHint(target, mode);
		}
		
		[AutoGenerated(Category = "Version15", Version = "1.5", EntryPoint = "glIsBuffer")]
		public static bool IsBuffer(Int32 buffer) {
			return Delegates.glIsBuffer((UInt32)buffer);
		}
		
		[AutoGenerated(Category = "Version11", Version = "1.1", EntryPoint = "glIsTexture")]
		public static bool IsTexture(Int32 texture) {
			return Delegates.glIsTexture((UInt32)texture);
		}
		
		[AutoGenerated(Category = "Version10Deprecated", Version = "1.0", EntryPoint = "glLoadIdentity")]
		public static void LoadIdentity() {
			Delegates.glLoadIdentity();
		}

		[AutoGenerated(Category = "Version10Deprecated", Version = "1.0", EntryPoint = "glLoadMatrixf")]
		public static unsafe void LoadMatrix(Single[] m) {
			fixed (Single* m_ptr = m) {
				Delegates.glLoadMatrixf(m_ptr);
			}
		}

		[AutoGenerated(Category = "Version10Deprecated", Version = "1.0", EntryPoint = "glLoadMatrixf")]
		public static unsafe void LoadMatrix(ref Single m) {
			fixed (Single* m_ptr = &m) {
				Delegates.glLoadMatrixf(m_ptr);
			}
		}

		[System.CLSCompliant(false)]
		[AutoGenerated(Category = "Version10Deprecated", Version = "1.0", EntryPoint = "glLoadMatrixf")]
		public static unsafe void LoadMatrix(Single* m) {
			Delegates.glLoadMatrixf(m);
		}
		
		[AutoGenerated(Category = "Version10Deprecated", Version = "1.0", EntryPoint = "glMatrixMode")]
		public static void MatrixMode(MatrixMode mode) {
			Delegates.glMatrixMode(mode);
		}
		
		[AutoGenerated(Category = "Version10Deprecated", Version = "1.0", EntryPoint = "glMultMatrixf")]
		public static unsafe void MultMatrix(Single[] m) {
			fixed (Single* m_ptr = m) {
				Delegates.glMultMatrixf(m_ptr);
			}
		}

		[AutoGenerated(Category = "Version10Deprecated", Version = "1.0", EntryPoint = "glMultMatrixf")]
		public static unsafe void MultMatrix(ref Single m) {
			fixed (Single* m_ptr = &m) {
				Delegates.glMultMatrixf(m_ptr);
			}
		}
		
		[System.CLSCompliant(false)]
		[AutoGenerated(Category = "Version10Deprecated", Version = "1.0", EntryPoint = "glMultMatrixf")]
		public static unsafe void MultMatrix(Single* m) {
			Delegates.glMultMatrixf(m);
		}

		[AutoGenerated(Category = "Version10", Version = "1.0", EntryPoint = "glPolygonMode")]
		public static void PolygonMode(MaterialFace face, PolygonMode mode) {
			Delegates.glPolygonMode(face, mode);
		}

		[AutoGenerated(Category = "Version11", Version = "1.1", EntryPoint = "glPolygonOffset")]
		public static void PolygonOffset(Single factor, Single units) {
			Delegates.glPolygonOffset(factor, units);
		}

		[AutoGenerated(Category = "Version10Deprecated", Version = "1.0", EntryPoint = "glPopMatrix")]
		public static void PopMatrix() {
			Delegates.glPopMatrix();
		}
		
		[AutoGenerated(Category = "Version10Deprecated", Version = "1.0", EntryPoint = "glPushMatrix")]
		public static void PushMatrix() {
			Delegates.glPushMatrix();
		}

		[AutoGenerated(Category = "Version10", Version = "1.0", EntryPoint = "glReadPixels")]
		public static void ReadPixels(Int32 x, Int32 y, Int32 width, Int32 height, PixelFormat format, PixelType type, [OutAttribute] IntPtr pixels) {
			Delegates.glReadPixels(x, y, width, height, format, type, pixels);
		}
		
		[AutoGenerated(Category = "Version10Deprecated", Version = "1.0", EntryPoint = "glRotatef")]
		public static void Rotate(Single angle, Single x, Single y, Single z) {
			Delegates.glRotatef(angle, x, y, z);
		}
		
		[AutoGenerated(Category = "Version10Deprecated", Version = "1.0", EntryPoint = "glScalef")]
		public static void Scale(Single x, Single y, Single z) {
			Delegates.glScalef(x, y, z);
		}
		
		[AutoGenerated(Category = "Version10Deprecated", Version = "1.0", EntryPoint = "glShadeModel")]
		public static void ShadeModel(ShadingModel mode) {
			Delegates.glShadeModel(mode);
		}
		
		[AutoGenerated(Category = "Version11Deprecated", Version = "1.1", EntryPoint = "glTexCoordPointer")]
		public static void TexCoordPointer(Int32 size, TexCoordPointerType type, Int32 stride, IntPtr pointer) {
			Delegates.glTexCoordPointer(size, type, stride, pointer);
		}


		[AutoGenerated(Category = "Version10", Version = "1.0", EntryPoint = "glTexImage2D")]
		public static void TexImage2D(TextureTarget target, Int32 level, PixelInternalFormat internalformat, Int32 width, Int32 height, Int32 border, PixelFormat format, PixelType type, IntPtr pixels) {
			Delegates.glTexImage2D(target, level, internalformat, width, height, border, format, type, pixels);
		}

		[AutoGenerated(Category = "Version10", Version = "1.0", EntryPoint = "glTexParameterf")]
		public static void TexParameter(TextureTarget target, TextureParameterName pname, Single param) {
			Delegates.glTexParameterf(target, pname, param);
		}

		[AutoGenerated(Category = "Version10", Version = "1.0", EntryPoint = "glTexParameterfv")]
		public static unsafe void TexParameter(TextureTarget target, TextureParameterName pname, Single[] @params) {
			fixed (Single* @params_ptr = @params) {
				Delegates.glTexParameterfv(target, pname, @params_ptr);
			}
		}

		[System.CLSCompliant(false)]
		[AutoGenerated(Category = "Version10", Version = "1.0", EntryPoint = "glTexParameterfv")]
		public static unsafe void TexParameter(TextureTarget target, TextureParameterName pname, Single* @params) {
			Delegates.glTexParameterfv(target, pname, @params);
		}

		[AutoGenerated(Category = "Version10", Version = "1.0", EntryPoint = "glTexParameteri")]
		public static void TexParameter(TextureTarget target, TextureParameterName pname, Int32 param) {
			Delegates.glTexParameteri(target, pname, param);
		}

		[AutoGenerated(Category = "Version10", Version = "1.0", EntryPoint = "glTexParameteriv")]
		public static unsafe void TexParameter(TextureTarget target, TextureParameterName pname, Int32[] @params) {
			fixed (Int32* @params_ptr = @params) {
				Delegates.glTexParameteriv(target, pname, @params_ptr);
			}
		}

		[System.CLSCompliant(false)]
		[AutoGenerated(Category = "Version10", Version = "1.0", EntryPoint = "glTexParameteriv")]
		public static unsafe void TexParameter(TextureTarget target, TextureParameterName pname, Int32* @params) {
			Delegates.glTexParameteriv(target, pname, @params);
		}
		
		[AutoGenerated(Category = "Version11", Version = "1.1", EntryPoint = "glTexSubImage2D")]
		public static void TexSubImage2D(TextureTarget target, Int32 level, Int32 xoffset, Int32 yoffset, Int32 width, Int32 height, PixelFormat format, PixelType type, IntPtr pixels) {
			Delegates.glTexSubImage2D(target, level, xoffset, yoffset, width, height, format, type, pixels);
		}
		
		[AutoGenerated(Category = "Version10Deprecated", Version = "1.0", EntryPoint = "glTranslatef")]
		public static void Translate(Single x, Single y, Single z) {
			Delegates.glTranslatef(x, y, z);
		}
		
		[AutoGenerated(Category = "Version11Deprecated", Version = "1.1", EntryPoint = "glVertexPointer")]
		public static void VertexPointer(Int32 size, VertexPointerType type, Int32 stride, IntPtr pointer) {
			Delegates.glVertexPointer(size, type, stride, pointer);
		}
		
		[AutoGenerated(Category = "Version10", Version = "1.0", EntryPoint = "glViewport")]
		public static void Viewport(Int32 x, Int32 y, Int32 width, Int32 height) {
			Delegates.glViewport(x, y, width, height);
		}
	}
}
