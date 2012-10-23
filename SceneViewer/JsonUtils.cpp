#include "JsonUtils.h"

using namespace std;

const Json::Value JsonUtils::GetRequiredJsonProp(const Json::Value& parent, const string& propname)
{
	const Json::Value val = parent[propname];
	if (val.isNull())
		FatalError(string("Could not find a required JSON property named '" + propname + "'"))
	return val;
}

void JsonUtils::ParseFloatArrayProp(const Json::Value prop, vector<float>& out)
{
	out.resize(prop.size());
	for (UINT i = 0; i < prop.size(); i++)
	{
		out[i] = (float)(prop[i].asDouble());
	}
}

void JsonUtils::ParseIntArrayProp(const Json::Value prop, vector<int>& out)
{
	out.resize(prop.size());
	for (UINT i = 0; i < prop.size(); i++)
	{
		out[i] = prop[i].asInt();
	}
}
