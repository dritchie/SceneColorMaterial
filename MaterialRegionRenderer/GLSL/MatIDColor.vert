uniform mat4 ModelViewMatrix;
uniform mat4 ProjectionMatrix;

in vec3 InVertex;

void main()
{
	gl_Position = ProjectionMatrix * ModelViewMatrix * vec4(InVertex, 1.0);
}