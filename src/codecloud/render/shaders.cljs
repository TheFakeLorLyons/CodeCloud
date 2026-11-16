(ns codecloud.render.shaders
  "GLSL for the backdrops, the folder clouds, the namespace pages, and the
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

(def backdrop-vertex
  "A quad over the whole screen, behind everything, for the backdrops.
  Each looks up what it shows along the view direction of its pixel."
  "varying vec2 screenPosition;
   void main() {
     screenPosition = position.xy;
     gl_Position = vec4(position.xy, 0.9999, 1.0);
   }")

(def sky-fragment
  "The sky looked up along each pixel's view direction, with cumulus above
  the horizon, a sea of cloud below, the sun by day, and moon, stars, and
  aurora by night, crossed by birds and airplanes."
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

(def plain-fragment
  "The plain backdrop, a gradient from horizon to zenith in the sky's day
  or night colors."
  "uniform mat4 inverseProjection;
   uniform mat4 cameraWorld;
   uniform float night;
   uniform vec3 dayZenith;
   uniform vec3 dayHorizon;
   uniform vec3 nightZenith;
   uniform vec3 nightHorizon;
   varying vec2 screenPosition;
   void main() {
     vec4 viewRay = inverseProjection * vec4(screenPosition, 1.0, 1.0);
     vec3 direction = normalize((cameraWorld * vec4(viewRay.xyz / viewRay.w, 0.0)).xyz);
     float rise = smoothstep(-0.6, 0.9, direction.y);
     vec3 dayColor = mix(dayHorizon, dayZenith, rise);
     vec3 nightColor = mix(nightHorizon, nightZenith, rise);
     gl_FragColor = vec4(mix(dayColor, nightColor, night), 1.0);
   }")

(def room-fragment
  "A room or a maze of rooms around an eye 1.5 meters above the floor,
  walked cell by cell until a wall stops the view. Patterns follow
  codecloud.presentation.environments/patterns and even out where finer
  than a pixel."
  (str
   "uniform mat4 inverseProjection;
    uniform mat4 cameraWorld;
    uniform float time;
    uniform float motion;
    uniform float pixelAngle;
    uniform float maze;
    uniform vec3 wallColor;
    uniform vec3 wallAccent;
    uniform float wallPattern;
    uniform float wallScale;
    uniform vec3 floorColor;
    uniform vec3 floorAccent;
    uniform float floorPattern;
    uniform float floorScale;
    uniform vec3 ceilingColor;
    uniform vec3 ceilingAccent;
    uniform float ceilingPattern;
    uniform float ceilingScale;
    uniform vec3 lampColor;
    uniform float lampsOn;
    uniform vec3 hazeColor;
    varying vec2 screenPosition;
   " noise-functions "
    const float floorBelow = 1.5;
    const float ceilingAbove = 1.2;
    const float roomHeight = 2.7;
    const float cellSize = 4.0;
    const float roomHalf = 4.0;
    const float lampSpacing = 2.4;

    float evened(float blur) {
      return smoothstep(0.2, 0.6, blur);
    }

    float lines(float coord, float width, float blur) {
      float fromLine = abs(fract(coord + 0.5) - 0.5);
      float sharp = 1.0 - smoothstep(width * 0.5 - blur, width * 0.5 + blur, fromLine);
      return mix(sharp, width, evened(blur));
    }

    float bands(float coord, float blur) {
      float sharp = smoothstep(0.25 - blur, 0.25 + blur, abs(fract(coord) - 0.5));
      return mix(sharp, 0.5, evened(blur));
    }

    vec3 surface(float pattern, vec2 uv, vec3 base, vec3 accent, float scale, float footprint) {
      if (pattern < 0.5) {
        return base;
      }
      if (pattern < 1.5) {
        float period = 0.3 * scale;
        return mix(base, accent, bands(uv.x / period, footprint / period));
      }
      if (pattern < 2.5) {
        float period = 0.5 * scale;
        float blur = footprint / period;
        return mix(base, accent, max(lines(uv.x / period, 0.05, blur), lines(uv.y / period, 0.05, blur)));
      }
      if (pattern < 3.5) {
        float period = 0.6 * scale;
        float blur = footprint / period;
        float shade = (hash12(floor(uv / period)) - 0.5) * 0.07 * (1.0 - evened(blur));
        float grout = max(lines(uv.x / period, 0.06, blur), lines(uv.y / period, 0.06, blur));
        return mix(base * (1.0 + shade), accent, grout);
      }
      if (pattern < 4.5) {
        float period = 0.5 * scale;
        vec2 cell = floor(uv / period);
        float checks = mix(mod(cell.x + cell.y, 2.0), 0.5, evened(footprint / period));
        return mix(base, accent, checks);
      }
      if (pattern < 5.5) {
        float period = 0.3 * scale;
        float blur = footprint / period;
        float radius = 0.22;
        float spot = 1.0 - smoothstep(radius - blur, radius + blur, length(fract(uv / period) - 0.5));
        return mix(base, accent, mix(spot, 3.14159 * radius * radius, evened(blur)));
      }
      if (pattern < 6.5) {
        float fine = valueNoise(uv * 38.0 / scale) - 0.5;
        float fineShare = 1.0 - smoothstep(0.008, 0.03, footprint / scale);
        float damp = smoothstep(0.55, 0.8, softFbm(uv * 0.35 / scale + 3.7));
        vec3 woven = mix(base, accent, 0.35 + fine * 0.5 * fineShare);
        return mix(woven, accent * 0.82, damp * 0.6);
      }
      float period = 0.18 * scale;
      float blur = footprint / period;
      float stripes = max(lines(uv.x / period, 0.05, blur), lines(uv.x / period + 0.16, 0.05, blur)) * 0.8;
      vec2 motif = vec2(fract(uv.x / period + 0.08) - 0.5, fract(uv.y / (period * 1.6)) - 0.5);
      float diamond = 1.0 - smoothstep(0.16 - blur, 0.16 + blur, abs(motif.x) * 1.6 + abs(motif.y));
      diamond = mix(diamond, 0.05, evened(blur));
      float stain = smoothstep(0.55, 0.85, softFbm(vec2(uv.x * 0.4, uv.y * 1.3) / scale + 13.0))
                    * (1.0 - smoothstep(0.2, 1.4, uv.y));
      vec3 papered = mix(base, accent, max(stripes, diamond * 0.55));
      return mix(papered, accent * 0.75, stain * 0.5);
    }

    float wallOnEdge(vec2 edge, float across) {
      bool aroundEye = across < 0.5
                       ? (edge.y == 0.0 && (edge.x == 0.0 || edge.x == 1.0))
                       : (edge.x == 0.0 && (edge.y == 0.0 || edge.y == 1.0));
      if (aroundEye) {
        return 0.0;
      }
      return step(hash12(edge * 1.37 + vec2(across * 19.1, across * 7.3)), 0.42);
    }

    void main() {
      vec4 viewRay = inverseProjection * vec4(screenPosition, 1.0, 1.0);
      vec3 direction = normalize((cameraWorld * vec4(viewRay.xyz / viewRay.w, 0.0)).xyz);
      float reach = 400.0;
      float hitDistance = reach;
      float kind = 4.0;
      if (direction.y > 0.0001) {
        hitDistance = ceilingAbove / direction.y;
        kind = 1.0;
      } else if (direction.y < -0.0001) {
        hitDistance = floorBelow / -direction.y;
        kind = 0.0;
      }
      vec2 across = direction.xz;
      vec2 safeAcross = max(abs(across), vec2(0.00001));
      vec3 eye = vec3(0.0);
      if (maze < 0.5) {
        vec2 toWall = roomHalf / safeAcross;
        if (toWall.x < hitDistance && toWall.x <= toWall.y) {
          hitDistance = toWall.x;
          kind = 2.0;
        } else if (toWall.y < hitDistance) {
          hitDistance = toWall.y;
          kind = 3.0;
        }
      } else {
        eye = vec3(0.5 * cellSize, 0.0, 0.5 * cellSize);
        vec2 cell = vec2(0.0);
        vec2 stepSign = vec2(across.x >= 0.0 ? 1.0 : -1.0, across.y >= 0.0 ? 1.0 : -1.0);
        vec2 crossing = cellSize / safeAcross;
        vec2 nextCrossing = 0.5 * cellSize / safeAcross;
        for (int walk = 0; walk < 28; walk++) {
          if (min(nextCrossing.x, nextCrossing.y) >= hitDistance) {
            break;
          }
          if (nextCrossing.x < nextCrossing.y) {
            if (wallOnEdge(vec2(cell.x + max(stepSign.x, 0.0), cell.y), 0.0) > 0.5) {
              hitDistance = nextCrossing.x;
              kind = 2.0;
              break;
            }
            cell.x += stepSign.x;
            nextCrossing.x += crossing.x;
          } else {
            if (wallOnEdge(vec2(cell.x, cell.y + max(stepSign.y, 0.0)), 1.0) > 0.5) {
              hitDistance = nextCrossing.y;
              kind = 3.0;
              break;
            }
            cell.y += stepSign.y;
            nextCrossing.y += crossing.y;
          }
        }
      }

      vec3 hit = eye + direction * hitDistance;
      float height = hit.y + floorBelow;
      vec2 lampOffset = mod(hit.xz, lampSpacing) - 0.5 * lampSpacing;
      vec2 lampCell = floor(hit.xz / lampSpacing);
      float flicker = 1.0 - 0.55 * motion * step(0.985, hash12(lampCell + floor(time * 9.0)));
      float pool = exp(-dot(lampOffset, lampOffset) * 0.35) * lampsOn * flicker;
      vec3 color;
      float light;
      if (kind < 0.5) {
        float footprint = hitDistance * pixelAngle / max(-direction.y, 0.08);
        color = surface(floorPattern, hit.xz, floorColor, floorAccent, floorScale, footprint);
        light = 0.6 + 0.42 * pool;
      } else if (kind < 1.5) {
        float footprint = hitDistance * pixelAngle / max(direction.y, 0.08);
        color = surface(ceilingPattern, hit.xz, ceilingColor, ceilingAccent, ceilingScale, footprint);
        vec2 panel = abs(lampOffset) / vec2(0.6, 0.3);
        float panelEdge = max(panel.x, panel.y);
        float inPanel = 1.0 - smoothstep(0.96, 1.04, panelEdge);
        light = 0.74 + 0.16 * pool;
        color = mix(color * light, lampColor * (1.08 * flicker), inPanel * lampsOn);
        color += lampColor * 0.12 * exp(-max(panelEdge - 1.0, 0.0) * 2.5) * lampsOn * flicker * (1.0 - inPanel);
        light = 1.0;
      } else if (kind < 3.5) {
        float facing = kind < 2.5 ? abs(direction.x) : abs(direction.z);
        float footprint = hitDistance * pixelAngle / max(facing, 0.08);
        vec2 uv = vec2(kind < 2.5 ? hit.z : hit.x, height);
        color = surface(wallPattern, uv, wallColor, wallAccent, wallScale, footprint);
        float baseboard = 1.0 - smoothstep(0.09, 0.1, height);
        color = mix(color, wallAccent * 0.7, baseboard * 0.65);
        float corners = (0.76 + 0.24 * smoothstep(0.0, 0.35, height))
                        * (0.84 + 0.16 * smoothstep(0.0, 0.3, roomHeight - height));
        light = (0.8 + 0.3 * smoothstep(0.0, roomHeight, height) + 0.2 * pool) * corners;
      } else {
        color = hazeColor;
        light = 1.0;
      }
      float haze = 1.0 - exp(-hitDistance * 0.045);
      gl_FragColor = vec4(mix(color * light, hazeColor, haze), 1.0);
    }"))

(def folder-puff-vertex
  "Every folder cloud's puffs as camera-facing quads. A puff fades once it
  would cover much of the screen, so flying into a folder parts its
  cloud."
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
    uniform float plain;
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
      // A plain folder is a flat disc, lit from above, with no billows to compute.
      if (plain > 0.5) {
        float disc = 1.0 - smoothstep(0.8, 1.0, distanceFromCenter);
        float discLight = clamp(0.6 + 0.4 * cornerPosition.y, 0.0, 1.0);
        gl_FragColor = vec4(mix(shadeColor, lightColor, discLight), disc * alpha);
        return;
      }
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
  "A page's glass, accent edge, and name band, with its code drawn from
  the minimap as bars far away and colored words closer, fading once text
  tiles take over."
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
   attribute float dashed;
   uniform vec2 viewport;
   uniform vec3 cameraRight;
   uniform vec3 cameraUp;
   varying float alongEdge;
   varying float acrossEdge;
   varying vec4 color;
   varying float flowStrength;
   varying float dashCount;

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
     // A dash and its gap span six widths of the line, however long the line shows.
     vec4 clipStart = viewProjection * vec4(curvePoint(0.0), 1.0);
     vec4 clipEnd = viewProjection * vec4(curvePoint(1.0), 1.0);
     vec2 screenSpan = (clipEnd.xy / max(clipEnd.w, 0.0001) - clipStart.xy / max(clipStart.w, 0.0001)) * 0.5 * viewport;
     dashCount = dashed * max(1.0, length(screenSpan) / (6.0 * edgeWidth));
   }")

(def edge-fragment
  "uniform float time;
   uniform float motion;
   varying float alongEdge;
   varying float acrossEdge;
   varying vec4 color;
   varying float flowStrength;
   varying float dashCount;
   void main() {
     float softEdge = 1.0 - smoothstep(0.35, 1.0, abs(acrossEdge));
     float wave = fract(alongEdge * 2.5 - time * 0.32 * motion * (1.0 + flowStrength));
     float pulse = smoothstep(0.0, 0.06, wave) * (1.0 - smoothstep(0.06, 0.22, wave));
     float dashPhase = fract(alongEdge * dashCount);
     float dash = dashCount > 0.0 ? 1.0 - smoothstep(0.5, 0.62, dashPhase) : 1.0;
     float alpha = color.a * softEdge * dash * (0.55 + 0.45 * pulse * (0.4 + flowStrength));
     gl_FragColor = vec4(color.rgb + pulse * 0.18 * (0.4 + flowStrength), alpha);
   }")
