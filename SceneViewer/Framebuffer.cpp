#include "Framebuffer.h"
#include "Common/GL.h"


Framebuffer::Framebuffer()
	: framebuffer(0), renderbuffer(0), texture(0),
	  width(-1), height(-1)
{
}

Framebuffer::~Framebuffer()
{
	DeleteBufferState();
}

void Framebuffer::BindForDrawing(int w, int h)
{
	if (w != width || h != height)
	{
		DeleteBufferState();
		CreateBufferState(w, h);
	}

	glBindFramebuffer(GL_FRAMEBUFFER, framebuffer);
}

void Framebuffer::BindForReading()
{
	glBindFramebuffer(GL_FRAMEBUFFER, framebuffer);
}

void Framebuffer::Unbind()
{
	glBindFramebuffer(GL_FRAMEBUFFER, 0);
}

void Framebuffer::CreateBufferState(int w, int h)
{
	width = w;
	height = h;

	glGenFramebuffers(1, &framebuffer);
	glBindFramebuffer(GL_FRAMEBUFFER, framebuffer);

	glGenTextures(1, &texture);
	glBindTexture(GL_TEXTURE_2D, texture);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, w, h, 0, GL_RGBA, GL_UNSIGNED_BYTE, NULL);

	glGenRenderbuffers(1, &renderbuffer);
	glBindRenderbuffer(GL_RENDERBUFFER, renderbuffer);
	glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH_COMPONENT16, w, h);

	glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, texture, 0);
	glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, renderbuffer);

	// Unbind everything to restore default state
	glBindTexture(GL_TEXTURE_2D, 0);
	glBindRenderbuffer(GL_RENDERBUFFER, 0);
	glBindFramebuffer(GL_FRAMEBUFFER, 0);
}

void Framebuffer::DeleteBufferState()
{
	if (framebuffer)
		glDeleteFramebuffers(1, &framebuffer);
	if (renderbuffer)
		glDeleteRenderbuffers(1, &renderbuffer);
	if (texture)
		glDeleteTextures(1, &texture);
}