uniform mat4 ModelViewMatrix;
uniform mat4 ProjectionMatrix;

in vec3 InVertex;

uniform mat4 NormalMatrix;
in vec3 InNormal;
out vec3 Normal;

void main()
{
	gl_Position = ProjectionMatrix * ModelViewMatrix * vec4(InVertex, 1.0);
	Normal = (NormalMatrix * vec4(InNormal, 0.0)).xyz;
}