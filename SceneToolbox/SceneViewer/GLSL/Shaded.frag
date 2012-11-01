in vec3 Normal;

uniform vec3 Color;
uniform vec3 LightDir;

void main()
{
	// Simple, soft directional lighting.
    vec3 normal = normalize(Normal);
    vec3 light_vec = normalize(LightDir);
        
    float light = max(dot(normal, light_vec), 0.0);
    gl_FragColor = vec4(mix(1.0, light, 0.7)*Color.rgb, 1.0);
}