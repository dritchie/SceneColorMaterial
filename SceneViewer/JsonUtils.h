#pragma once

#ifndef __JSON_UTILS_H
#define __JSON_UTILS_H

#include "Common/Common.h"
#include "json/value.h"

class JsonUtils
{
public:
	static const Json::Value GetRequiredJsonProp(const Json::Value& parent, const std::string& propname);
	static void ParseFloatArrayProp(const Json::Value prop, std::vector<float>& out);
	static void ParseIntArrayProp(const Json::Value prop, std::vector<int>& out);
};


#endif
