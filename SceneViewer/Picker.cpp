#include "Picker.h"
#include "Common/GL.h"


Eigen::Vector4f Picker::PackIDs(unsigned short modelID, unsigned short compID)
{
	unsigned short idpair[2];
	idpair[0] = modelID; idpair[1] = compID;
	unsigned char *bytes = (unsigned char*)idpair;
	Eigen::Vector4f floats;
	floats[0] = bytes[0] / 255.0f;
	floats[1] = bytes[1] / 255.0f;
	floats[2] = bytes[2] / 255.0f;
	floats[3] = bytes[3] / 255.0f;
	return floats;
}

std::pair<unsigned short, unsigned short> Picker::UnpackIDs(unsigned char *pixel)
{
	unsigned short *shorts = (unsigned short*)pixel;
	return std::make_pair(shorts[0], shorts[1]);
}

std::pair<int, int> Picker::Pick(int x, int y)
{
	unsigned char pixel[4];
	
	pickBuffer.BindForReading();
	glReadPixels(x, y, 1, 1, GL_RGBA, GL_UNSIGNED_BYTE, pixel);
	pickBuffer.Unbind();

	// Subtract 1 from the model UID (so background pixels get the ID -1)
	std::pair<unsigned char, unsigned char> rawIDs = UnpackIDs(pixel);
	std::pair<int, int> ids;
	ids.first = ((int)rawIDs.first) - 1;
	ids.second = ((int)rawIDs.second);
	return ids;
}
