#version 330

in vec2 fragTexCoord;
in vec4 fragColor;

uniform sampler2D texture0;
uniform vec4 colDiffuse;

out vec4 finalColor;

uniform float renderWidth;
uniform float renderHeight;

float offset=0.;

uniform float time;

void main()
{
    float frequency=renderHeight/3.;
    
    float globalPos=(fragTexCoord.y+offset)*frequency;
    float wavePos=cos((fract(globalPos)-.5)*3.14);
    
    vec4 texelColor=texture(texture0,fragTexCoord);
    
    finalColor=mix(vec4(0.,.3,0.,0.),texelColor,wavePos);
}