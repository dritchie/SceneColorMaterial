#pragma once

#ifndef __FRAMEBUFFER_H
#define __FRAMEBUFFER_H

class Framebuffer
{
public:
	Framebuffer();
	~Framebuffer();
	void BindForDrawing(int w, int h);
	void BindForReading();
	void Unbind();

private:
	void CreateBufferState(int w, int h);
	void DeleteBufferState();

	int width, height;
	unsigned int framebuffer, renderbuffer, texture;
};

#endif