(ns codecloud.render.shaders
  "GLSL for the sky, the folder clouds, the namespace pages, and the
  dependency edges. Colors arrive as gamma-encoded sRGB vectors computed
  by the palette, and the shaders write them out unconverted.")

(def ^:private noise-functions
  "float hash12(vec2 point) {
     vec3 mixed = fract(vec3(point.xyx) * 0.1031);
     mixed += dot(mixed, mixed.yzx + 33.33);
     return fract((mixed.x + mixed.y) * mixed.z);
   }
   float hash13(vec3 point) {
     vec3 mixed = fract(point * 0.1031);
     mixed += dot(mixed, mixed.zyx + 31.32);
     return fract((mixed.x + mixed.y) * mixed.z);
   }
   float valueNoise(vec2 point) {
     vec2 cell = floor(point);
     vec2 local = fract(point);
     vec2 eased = local * local * (3.0 - 2.0 * local);
     return mix(mix(hash12(cell), hash12(cell + vec2(1.0, 0.0)), eased.x),
                mix(hash12(cell + vec2(0.0, 1.0)), hash12(cell + vec2(1.0, 1.0)), eased.x),
                eased.y);
   }
   float fbm(vec2 point) {
     float total = 0.0;
     float amplitude = 0.5;
     for (int octave = 0; octave < 5; octave++) {
       total += amplitude * valueNoise(point);
       point = point * 2.03 + vec2(17.1, 9.2);
       amplitude *= 0.5;
     }
     return total;
   }
   float softFbm(vec2 point) {
     float total = 0.0;
     float amplitude = 0.5;
     for (int octave = 0; octave < 3; octave++) {
       total += amplitude * valueNoise(point);
       point = point * 2.03 + vec2(17.1, 9.2);
       amplitude *= 0.5;
     }
     return total / 0.875;
   }")

(def sky-vertex
  "varying vec2 screenPosition;
   void main() {
     screenPosition = position.xy;
     gl_Position = vec4(position.xy, 0.9999, 1.0);
   }")

(def sky-fragment
  "The sky behind the cloud, looked up along each pixel's view direction,
  with cumulus above the horizon, a sea of cloud below it, the sun by day,
  and the moon, stars, and an aurora by night. Birds fly as flapping
  strokes, and an airplane draws a contrail by day and blinks by night."
  (str
   "uniform mat4 inverseProjection;
    uniform mat4 cameraWorld;
    uniform float time;
    uniform float night;
    uniform float dusk;
    uniform vec3 dayZenith;
    uniform vec3 dayHorizon;
    uniform vec3 duskZenith;
    uniform vec3 duskHorizon;
    uniform vec3 nightZenith;
    uniform vec3 nightHorizon;
    uniform vec3 sunColor;
    uniform vec3 moonColor;
    uniform vec3 auroraColor;
    uniform vec3 sunDirection;
    uniform vec3 moonDirection;
    uniform vec3 flockCenter;
    uniform float flockHeading;
    uniform float flockScale;
    uniform float flockAlpha;
    uniform float flapTime;
    uniform vec3 planeHead;
    uniform vec3 planeTail;
    uniform float planeScale;
    uniform float planeAlpha;
    varying vec2 screenPosition;
   " noise-functions "
    float cumulus(vec2 point, float coverage) {
      float shape = fbm(point);
      float detail = valueNoise(point * 6.3 + vec2(3.1, 7.7));
      float threshold = 1.0 - coverage;
      return smoothstep(threshold, threshold + 0.2, shape * 0.85 + detail * 0.15);
    }

    float segmentDistance(vec2 point, vec2 start, vec2 end) {
      vec2 along = end - start;
      float share = clamp(dot(point - start, along) / dot(along, along), 0.0, 1.0);
      return length(point - start - along * share);
    }

    float birdDistance(vec2 local, float flap) {
      float elbowLift = mix(0.12, 0.34, flap);
      float tipLift = mix(-0.2, 0.62, flap);
      vec2 leftElbow = vec2(-0.48, elbowLift);
      vec2 rightElbow = vec2(0.48, elbowLift);
      float left = min(segmentDistance(local, vec2(0.0), leftElbow),
                       segmentDistance(local, leftElbow, vec2(-1.0, tipLift)));
      float right = min(segmentDistance(local, vec2(0.0), rightElbow),
                        segmentDistance(local, rightElbow, vec2(1.0, tipLift)));
      return min(left, right);
    }

    void main() {
      vec4 viewRay = inverseProjection * vec4(screenPosition, 1.0, 1.0);
      vec3 direction = normalize((cameraWorld * vec4(viewRay.xyz / viewRay.w, 0.0)).xyz);
      float elevation = direction.y;
      float daylight = 1.0 - night;

      vec3 dayColor = mix(dayHorizon, dayZenith, smoothstep(-0.55, 0.95, elevation));
      vec3 duskColor = mix(duskHorizon, duskZenith, smoothstep(-0.4, 0.9, elevation));
      vec3 nightColor = mix(nightHorizon, nightZenith, smoothstep(-0.5, 0.9, elevation));
      vec3 color = mix(mix(dayColor, nightColor, night), duskColor, dusk * 0.85);
      vec3 horizonColor = mix(mix(dayHorizon, nightHorizon, night), duskHorizon, dusk * 0.85);

      float sunAlignment = max(dot(direction, sunDirection), 0.0);
      float sunDisc = smoothstep(0.99984, 0.9999, sunAlignment);
      float sunGlow = pow(sunAlignment, 900.0) * 0.28 + pow(sunAlignment, 60.0) * 0.07;
      color += sunColor * (sunDisc * 0.85 + sunGlow) * daylight;

      float moonAlignment = max(dot(direction, moonDirection), 0.0);
      float moonAbove = smoothstep(-0.04, 0.01, elevation);
      float moonDisc = smoothstep(0.99968, 0.9998, moonAlignment) * moonAbove;
      color += moonColor * (moonDisc * 0.8 + pow(moonAlignment, 90.0) * 0.08 * moonAbove) * night;

      if (night > 0.001 && elevation > -0.02) {
        vec3 starGrid = direction * 170.0;
        vec3 starCell = floor(starGrid);
        float starSeed = hash13(starCell);
        vec3 starPoint = vec3(hash13(starCell + 1.7), hash13(starCell + 3.1), hash13(starCell + 5.3));
        float starDistance = length(fract(starGrid) - starPoint);
        float twinkle = 0.65 + 0.35 * sin(time * (1.3 + starSeed * 2.3) + starSeed * 40.0);
        float star = step(0.9862, starSeed) * smoothstep(0.17, 0.0, starDistance) * twinkle;
        color += vec3(star) * night * (1.0 - moonDisc) * smoothstep(-0.02, 0.12, elevation);

        float auroraBand = smoothstep(-0.02, 0.12, elevation) * (1.0 - smoothstep(0.15, 0.5, elevation));
        if (auroraBand > 0.0) {
          float auroraCurtain = fbm(vec2(atan(direction.z, direction.x) * 3.0, elevation * 2.0 + time * 0.015));
          color += auroraColor * auroraBand * smoothstep(0.5, 0.85, auroraCurtain) * 0.18 * night;
        }
      }

      vec3 litDay = vec3(1.0, 0.995, 0.985);
      vec3 litNight = nightHorizon * 1.55 + moonColor * 0.06;
      vec3 litDusk = mix(duskHorizon, vec3(1.0, 0.9, 0.84), 0.55);
      vec3 cloudLit = mix(mix(litDay, litNight, night), litDusk, dusk * 0.8);
      vec3 cloudShade = horizonColor * mix(0.86, 0.72, night);
      vec2 towardSun = normalize(sunDirection.xz + vec2(0.0001, 0.0));
      float cover;
      float light;
      if (elevation > 0.0) {
        vec2 cloudPoint = direction.xz / (elevation + 0.1) * 1.4 + vec2(time * 0.008, time * 0.003);
        float density = cumulus(cloudPoint, 0.4);
        float sunward = cumulus(cloudPoint + towardSun * 0.12, 0.4);
        light = clamp(0.78 + (density - sunward) * 0.9 + 0.2 * (1.0 - density), 0.0, 1.0);
        cover = density * smoothstep(0.0, 0.16, elevation) * 0.85;
      } else {
        float below = -elevation;
        vec2 seaPoint = direction.xz / (below + 0.06) * 0.8 + vec2(time * 0.005, -time * 0.002);
        float density = cumulus(seaPoint, 0.58);
        float sunward = cumulus(seaPoint + towardSun * 0.1, 0.58);
        light = clamp(0.82 + (density - sunward) * 0.8, 0.0, 1.0);
        cover = density * smoothstep(0.0, 0.2, below) * 0.72;
      }
      color = mix(color, mix(cloudShade, cloudLit, light), cover);
      color += sunColor * sunGlow * 0.4 * daylight * cover;

      float haze = 1.0 - smoothstep(0.0, 0.14, abs(elevation));
      color = mix(color, horizonColor * mix(1.04, 1.0, night), haze * 0.35);

      vec3 east = normalize(cross(flockCenter, vec3(0.0, 1.0, 0.0)) + vec3(0.0001, 0.0, 0.0));
      vec3 north = cross(east, flockCenter);
      vec2 flockLocal = vec2(dot(direction, east), dot(direction, north)) / flockScale;
      float birdPixel = fwidth(flockLocal.x) + fwidth(flockLocal.y);
      if (flockAlpha > 0.001 && dot(direction, flockCenter) > 0.95) {
        float birds = 0.0;
        for (int bird = 0; bird < 7; bird++) {
          float rank = float(bird);
          float row = floor((rank + 1.0) * 0.5);
          float side = mod(rank, 2.0) * 2.0 - 1.0;
          vec2 offset = vec2(-flockHeading * row * 2.8,
                             side * row * 1.25 + 0.3 * sin(flapTime * 0.7 + rank * 2.1));
          float flap = 0.5 + 0.5 * sin(flapTime * 6.0 + rank * 1.9);
          float stroke = birdDistance(flockLocal - offset, flap);
          birds = max(birds, 1.0 - smoothstep(0.12, 0.12 + birdPixel * 1.2, stroke));
        }
        color = mix(color, vec3(0.16, 0.18, 0.24), birds * flockAlpha * 0.85);
      }

      if (planeAlpha > 0.001 && dot(direction, planeHead) > 0.0) {
        vec3 arcNormal = normalize(cross(planeTail, planeHead) + vec3(0.0, 0.00001, 0.0));
        float offArc = abs(dot(direction, arcNormal));
        float fromTail = dot(cross(planeTail, direction), arcNormal);
        float toHead = dot(cross(direction, planeHead), arcNormal);
        float span = max(length(cross(planeTail, planeHead)), 0.0001);
        if (fromTail > 0.0 && toHead > 0.0) {
          float age = clamp(toHead / span, 0.0, 1.0);
          float width = mix(0.0006, 0.003, age);
          float trail = (1.0 - smoothstep(width * 0.4, width, offArc))
                        * smoothstep(0.0, 0.02, age) * (1.0 - age);
          color = mix(color, vec3(1.0), trail * planeAlpha * daylight * 0.85);
        }
        float headDistance = length(direction - planeHead);
        float speck = 1.0 - smoothstep(planeScale * 0.55, planeScale, headDistance);
        color = mix(color, vec3(0.34, 0.37, 0.44), speck * planeAlpha * daylight);
        float blink = step(0.82, fract(time * 0.9));
        float strobe = step(0.92, fract(time * 0.75 + 0.4));
        float lights = max(blink, strobe) * (1.0 - smoothstep(planeScale * 0.3, planeScale * 1.8, headDistance));
        color += mix(vec3(1.0, 0.28, 0.22), vec3(1.0), strobe) * lights * planeAlpha * night;
      }

      gl_FragColor = vec4(color, 1.0);
    }"))

(def folder-puff-vertex
  "Puffs of every folder cloud in one mesh, each a quad facing the camera.
  A puff fades once it would cover much of the screen, so flying into a
  folder parts its cloud, and its height within the cloud, from -1 at the
  base to 1 at the top, lets the cloud be lit as one body."
  "attribute vec3 puffCenter;
   attribute vec2 corner;
   attribute float puffSize;
   attribute float puffSeed;
   attribute float puffAlpha;
   attribute vec3 puffLight;
   attribute vec3 puffShade;
   attribute vec4 cloudFrame;
   uniform float time;
   uniform float motion;
   varying vec2 cornerPosition;
   varying float seed;
   varying float alpha;
   varying float heightInCloud;
   varying vec3 lightColor;
   varying vec3 shadeColor;
   void main() {
     vec4 viewCenter = viewMatrix * vec4(puffCenter, 1.0);
     viewCenter.y += sin(time * 0.21 + puffSeed * 6.2831) * 0.025 * puffSize * motion;
     float depth = max(-viewCenter.z, 0.001);
     float screenShare = puffSize * projectionMatrix[1][1] / depth;
     viewCenter.xy += corner * puffSize;
     vec4 viewCloud = viewMatrix * vec4(cloudFrame.xyz, 1.0);
     heightInCloud = (viewCenter.y - viewCloud.y) / max(cloudFrame.w, 0.001);
     cornerPosition = corner;
     seed = puffSeed;
     lightColor = puffLight;
     shadeColor = puffShade;
     alpha = puffAlpha * (1.0 - smoothstep(0.7, 1.9, screenShare)) * step(0.0, -viewCenter.z);
     gl_Position = projectionMatrix * viewCenter;
   }")

(def folder-puff-fragment
  (str
   "uniform float time;
    uniform float motion;
    varying vec2 cornerPosition;
    varying float seed;
    varying float alpha;
    varying float heightInCloud;
    varying vec3 lightColor;
    varying vec3 shadeColor;
   " noise-functions "
    void main() {
      float distanceFromCenter = length(cornerPosition);
      if (distanceFromCenter > 1.0 || alpha < 0.002) discard;
      float billow = softFbm(cornerPosition * 2.1 + vec2(seed * 7.3, time * 0.018 * motion));
      float edge = distanceFromCenter + 0.32 * (billow - 0.5);
      float body = 1.0 - smoothstep(0.62, 0.86, edge);
      float cloudLight = smoothstep(-0.95, 0.6, heightInCloud);
      float puffLight = clamp(0.55 + 0.45 * cornerPosition.y + 0.25 * billow, 0.0, 1.0);
      float light = mix(cloudLight, puffLight, 0.35);
      // A shaded band inside each puff's edge, deeper on its underside,
      // outlines the billows where puffs overlap.
      float band = smoothstep(0.42, 0.78, edge) * (1.0 - smoothstep(0.78, 0.86, edge));
      float underside = clamp(0.6 - 0.6 * cornerPosition.y, 0.0, 1.0);
      float crevice = band * (0.45 + 0.55 * underside);
      vec3 color = mix(mix(shadeColor, lightColor, light), shadeColor, 0.55 * crevice);
      gl_FragColor = vec4(color, body * alpha);
    }"))

(def page-vertex
  "varying vec2 pageUv;
   void main() {
     pageUv = uv;
     gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0);
   }")

(def page-fragment
  "Draws a page's glass as a rounded panel with an accent edge and a name
  band, and the shape of its code from the minimap table. Far away the
  code shows as bars and closer as colored words, and the runs fade out
  once text tiles take over."
  "uniform vec2 size;
   uniform vec3 panelColor;
   uniform vec3 accentColor;
   uniform vec3 inkColor;
   uniform float opacity;
   uniform float headerHeight;
   uniform float padding;
   uniform float columnWidth;
   uniform float columnGap;
   uniform float gutterWidth;
   uniform float characterWidth;
   uniform float rows;
   uniform float columns;
   uniform float lineCount;
   uniform float codeCharacters;
   uniform sampler2D minimap;
   uniform vec2 minimapSize;
   uniform float minimapOpacity;
   uniform float hasMinimap;
   uniform vec3 syntaxColors[10];
   uniform float highlight;
   uniform float pulse;
   uniform float pill;
   uniform float cornerRadius;
   uniform float borderWidth;
   uniform float borderPixels;
   varying vec2 pageUv;

   float roundedBoxDistance(vec2 point, vec2 halfSize, float radius) {
     vec2 inner = abs(point) - halfSize + radius;
     return length(max(inner, 0.0)) + min(max(inner.x, inner.y), 0.0) - radius;
   }

   vec4 runAt(float line, float slot) {
     vec2 texel = vec2(mod(line, 16.0) * 16.0 + slot, floor(line / 16.0));
     return texture2D(minimap, (texel + 0.5) / minimapSize) * 255.0;
   }

   void main() {
     vec2 local = (pageUv - 0.5) * size;
     vec2 halfSize = 0.5 * size;
     float radius = min(cornerRadius, 0.5 * min(size.x, size.y));
     float edgeDistance = roundedBoxDistance(local, halfSize, radius);
     float pixelWidth = max(fwidth(edgeDistance), 0.00001);
     float coverage = 1.0 - smoothstep(-pixelWidth, pixelWidth, edgeDistance);
     if (coverage <= 0.001) discard;

     vec3 color = panelColor;
     float fromTop = halfSize.y - local.y;
     float header = (1.0 - pill) * (1.0 - smoothstep(padding + headerHeight - 0.2, padding + headerHeight + 0.2, fromTop));
     color = mix(color, accentColor, 0.34 * header);

     if (hasMinimap > 0.5 && pill < 0.5 && minimapOpacity > 0.001) {
       vec2 codePoint = vec2(local.x + halfSize.x - padding, fromTop - padding - headerHeight);
       float stride = columnWidth + columnGap;
       float column = floor(codePoint.x / stride);
       float inColumn = codePoint.x - column * stride;
       float row = floor(codePoint.y);
       float line = column * rows + row;
       if (codePoint.y >= 0.0 && row < rows && column >= 0.0 && column < columns
           && inColumn < columnWidth && line < lineCount) {
         float linePixels = 1.0 / max(fwidth(codePoint.y), 0.00001);
         float character = (inColumn - gutterWidth) / characterWidth;
         if (character >= 0.0 && character < codeCharacters) {
           vec4 extent = runAt(line, 15.0);
           float bar = step(extent.r, character) * step(character, extent.g);
           float near = smoothstep(1.4, 3.4, linePixels);
           color = mix(color, inkColor, bar * 0.24 * (1.0 - near) * minimapOpacity);
           if (near > 0.0 && bar > 0.0) {
             float band = mix(1.0,
                              smoothstep(0.2, 0.28, fract(codePoint.y)) * (1.0 - smoothstep(0.72, 0.8, fract(codePoint.y))),
                              smoothstep(3.0, 6.0, linePixels));
             for (int slot = 0; slot < 15; slot++) {
               vec4 run = runAt(line, float(slot));
               if (run.g < 0.5 || character < run.r) break;
               if (character < run.r + run.g) {
                 color = mix(color, syntaxColors[int(run.b + 0.5)], band * near * minimapOpacity * 0.62);
                 break;
               }
             }
           }
         }
       }
     }

     float borderBand = max(borderWidth, borderPixels * pixelWidth);
     float border = smoothstep(-borderBand - pixelWidth, -borderBand + pixelWidth, edgeDistance);
     color = mix(color, accentColor, border);
     color = mix(color, accentColor * 1.12, border * pulse * 0.45);
     color += vec3(highlight * 0.05);
     float alpha = mix(opacity, min(1.0, opacity * 1.2 + 0.18), border);
     gl_FragColor = vec4(color, alpha * coverage);
   }")

(def edge-vertex
  "attribute vec3 edgeStart;
   attribute vec3 edgeEnd;
   attribute vec2 endRadii;
   attribute float bend;
   attribute float progress;
   attribute float side;
   attribute vec4 startColor;
   attribute vec3 endColor;
   attribute float edgeWidth;
   attribute float flow;
   uniform vec2 viewport;
   uniform vec3 cameraRight;
   uniform vec3 cameraUp;
   varying float alongEdge;
   varying float acrossEdge;
   varying vec4 color;
   varying float flowStrength;

   vec3 curvePoint(float fraction) {
     vec3 span = edgeEnd - edgeStart;
     float spanLength = length(span);
     if (spanLength < 0.001) {
       float angle = fraction * 6.2831853;
       float loopSize = endRadii.x * 0.9;
       return edgeStart + cameraUp * endRadii.x * 0.8
              + (cameraRight * sin(angle) + cameraUp * (1.0 - cos(angle))) * loopSize;
     }
     float startTrim = min(0.45, endRadii.x / spanLength);
     float endTrim = max(0.55, 1.0 - endRadii.y / spanLength);
     float trimmed = mix(startTrim, endTrim, fraction);
     vec3 sideways = normalize(cross(span, vec3(0.0, 1.0, 0.0)) + vec3(0.0001, 0.0, 0.0));
     vec3 control = mix(edgeStart, edgeEnd, 0.5) + sideways * bend * spanLength
                    + vec3(0.0, 0.06 * spanLength, 0.0);
     float remaining = 1.0 - trimmed;
     return remaining * remaining * edgeStart + 2.0 * remaining * trimmed * control + trimmed * trimmed * edgeEnd;
   }

   void main() {
     vec3 here = curvePoint(progress);
     vec3 ahead = curvePoint(min(progress + 0.015, 1.0));
     vec3 behind = curvePoint(max(progress - 0.015, 0.0));
     mat4 viewProjection = projectionMatrix * viewMatrix;
     vec4 clipHere = viewProjection * vec4(here, 1.0);
     vec4 clipAhead = viewProjection * vec4(ahead, 1.0);
     vec4 clipBehind = viewProjection * vec4(behind, 1.0);
     vec2 screenAhead = clipAhead.xy / max(clipAhead.w, 0.0001) * viewport;
     vec2 screenBehind = clipBehind.xy / max(clipBehind.w, 0.0001) * viewport;
     vec2 direction = screenAhead - screenBehind;
     direction = length(direction) > 0.0001 ? normalize(direction) : vec2(1.0, 0.0);
     vec2 normal = vec2(-direction.y, direction.x);
     clipHere.xy += normal * side * edgeWidth / viewport * clipHere.w;
     gl_Position = clipHere;
     alongEdge = progress;
     acrossEdge = side;
     color = vec4(mix(startColor.rgb, endColor, smoothstep(0.08, 0.92, progress)), startColor.a);
     flowStrength = flow;
   }")

(def edge-fragment
  "uniform float time;
   uniform float motion;
   varying float alongEdge;
   varying float acrossEdge;
   varying vec4 color;
   varying float flowStrength;
   void main() {
     float softEdge = 1.0 - smoothstep(0.35, 1.0, abs(acrossEdge));
     float wave = fract(alongEdge * 2.5 - time * 0.32 * motion * (1.0 + flowStrength));
     float pulse = smoothstep(0.0, 0.06, wave) * (1.0 - smoothstep(0.06, 0.22, wave));
     float alpha = color.a * softEdge * (0.55 + 0.45 * pulse * (0.4 + flowStrength));
     gl_FragColor = vec4(color.rgb + pulse * 0.18 * (0.4 + flowStrength), alpha);
   }")
