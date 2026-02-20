-- sphere_widget_all_rot_around_bottom_grouped_ui.lua
-- Aseprite 1.3.x compatible.
-- All rotations (roll, yaw, pitch) pivot around the selected roll-side (bottom/top/left/right),
-- OR the sphere center if none is selected.
--
-- Presets in script folder:
--   PRESET: *.txt  (SHAPE + POSE in one file)
--   (Ignores legacy: *.shape.txt / *.pose.txt / *.sphere.txt)
--
-- UI:
--   Save shape / Load shape  (reads/writes SHAPE keys only, inside the same file)
--   Save pose  / Load pose   (reads/writes POSE keys only, inside the same file)
--   NOTE: Load pose is still the only time radius is loaded.
--
-- Opacity rules (UPDATED):
--   - Front lines alpha = fg alpha * (front_alpha/255)
--   - Behind lines alpha = fg alpha * (back_alpha/255)
--   - Yaw/Pitch pad center crosshair uses same alpha as front wire
--
-- Back-face compositing rule:
--   - A lower-alpha pixel will NOT overwrite an existing higher-alpha pixel.
--     Prevents back-face lines from drawing over front opaque lines.
--
-- Back-face skip:
--   - If back alpha is 0, all back-face processing is skipped.
--
-- Apply behavior:
--   - Bakes ALL preview cels across frames into output layer (animation-friendly)

do
  local spr = app.activeSprite
  if not spr then return app.alert("No active sprite.") end

  local function clamp(v, a, b)
    if v < a then return a end
    if v > b then return b end
    return v
  end

  local function lerp(a, b, t) return a + (b - a) * t end

  local PREVIEW_NAME = "_SpherePreview"
  local OUTPUT_NAME  = "Sphere Wire"

  local function findLayerByName(name)
    for _, lyr in ipairs(spr.layers) do
      if lyr.name == name then return lyr end
    end
    return nil
  end

  local function ensureLayer(name)
    local lyr = findLayerByName(name)
    if not lyr then
      lyr = spr:newLayer()
      lyr.name = name
    end
    return lyr
  end

  local function deletePreviewLayer()
    local lyr = findLayerByName(PREVIEW_NAME)
    if lyr then spr:deleteLayer(lyr) end
  end

  local function replaceCel(layer, frame, img, pos)
    local old = layer:cel(frame)
    if old then spr:deleteCel(old) end
    return spr:newCel(layer, frame, img, pos or Point(0, 0))
  end

  local function deg2rad(d) return d * math.pi / 180 end

  -- Lua atan2 compatibility
  local function atan2(y, x)
    local ok, v = pcall(function() return math.atan(y, x) end)
    if ok and v ~= nil then return v end
    if x > 0 then
      return math.atan(y / x)
    elseif x < 0 and y >= 0 then
      return math.atan(y / x) + math.pi
    elseif x < 0 and y < 0 then
      return math.atan(y / x) - math.pi
    elseif x == 0 and y > 0 then
      return math.pi / 2
    elseif x == 0 and y < 0 then
      return -math.pi / 2
    end
    return 0
  end

  -- Back-face compositing guard: only draw if new alpha is stronger than existing alpha.
  local function drawPixelStrong(img, x, y, pix)
    if not pix then return end
    if x < 0 or y < 0 or x >= img.width or y >= img.height then return end
    local newA = app.pixelColor.rgbaA(pix)
    if newA <= 0 then return end
    local cur = img:getPixel(x, y)
    local curA = app.pixelColor.rgbaA(cur)
    if curA >= newA then return end
    img:drawPixel(x, y, pix)
  end

  local function drawLineSet(img, x0, y0, x1, y1, pix)
    if not pix then return end -- IMPORTANT: allows skipping backface entirely
    x0 = math.floor(x0 + 0.5); y0 = math.floor(y0 + 0.5)
    x1 = math.floor(x1 + 0.5); y1 = math.floor(y1 + 0.5)
    local dx = math.abs(x1 - x0)
    local sx = (x0 < x1) and 1 or -1
    local dy = -math.abs(y1 - y0)
    local sy = (y0 < y1) and 1 or -1
    local err = dx + dy
    while true do
      drawPixelStrong(img, x0, y0, pix)
      if x0 == x1 and y0 == y1 then break end
      local e2 = 2 * err
      if e2 >= dy then err = err + dy; x0 = x0 + sx end
      if e2 <= dx then err = err + dx; y0 = y0 + sy end
    end
  end

  local function getSelectionBounds()
    if spr.selection and not spr.selection.isEmpty then
      return spr.selection.bounds
    end
    return nil
  end

  local function getCenterFromBounds(b)
    if b then
      return b.x + math.floor(b.width / 2),
             b.y + math.floor(b.height / 2)
    end
    return math.floor(spr.width / 2), math.floor(spr.height / 2)
  end

  local function selectionMaxRadius(b)
    if not b then return nil end
    local d = math.min(b.width, b.height)
    return math.max(2, math.floor((d - 1) / 2))
  end

  -- =========================
  -- Presets in script folder
  -- =========================
  local function getScriptDir()
    local info = debug.getinfo(1, 'S')
    if not info or not info.source then return nil, "Could not determine script source path." end
    local src = info.source
    if src:sub(1,1) == '@' then src = src:sub(2) end
    local dir = app.fs.filePath(src)
    if dir == "" then return nil, "Script directory is empty or invalid." end
    return app.fs.normalizePath(dir), nil
  end

  local function parseValue(v)
    if v == "true" then return true end
    if v == "false" then return false end
    local n = tonumber(v)
    if n ~= nil then return n end
    return v
  end

  local function loadKeyValueFile(path)
    local t = {}
    local f = io.open(path, "r")
    if not f then return t, "Could not open: " .. tostring(path) end
    for line in f:lines() do
      local k, v = line:match("^%s*([^=]+)%s*=%s*(.-)%s*$")
      if k and v then t[k] = parseValue(v) end
    end
    f:close()
    return t, nil
  end

  local function saveKeyValueFile(path, data, keysInOrder)
    local f = io.open(path, "w")
    if not f then return false, "Could not write: " .. tostring(path) end
    local function w(k, v) f:write(tostring(k), "=", tostring(v), "\n") end
    if keysInOrder then
      for _, k in ipairs(keysInOrder) do
        if data[k] ~= nil then w(k, data[k]) end
      end
    else
      for k, v in pairs(data) do w(k, v) end
    end
    f:close()
    return true, nil
  end

  local SCRIPT_DIR, SD_ERR = getScriptDir()
  if not SCRIPT_DIR then return app.alert(SD_ERR or "Failed to locate script directory.") end

  local function presetPathForSelected(filename)
    return app.fs.joinPath(SCRIPT_DIR, filename)
  end

  local SHAPE_KEYS = {
    "cut_depth",
    "jaw_w","jaw_h","jaw_d",
    "nose_bh","nose_br","nose_th","nose_td",
    "chin_w","chin_h","chin_d",
    "show_outer","show_cross","show_cuts","show_face","show_jaws","show_chin","show_nose","show_ears",
    "ear_back","ear_rot","ear_x","ear_scale","ear_y","ear_z",
    "alpha_front","alpha_back",
    -- GROUP ENABLES must persist as SHAPE:
    "jawchin_en","nose_en","ears_en",
  }

  local POSE_KEYS = {"p1x","p1y","roll","radius_t","radius_px","roll_side","full_180"}

  local PRESET_KEYS = {}
  do
    local seen = {}
    for _,k in ipairs(SHAPE_KEYS) do if not seen[k] then PRESET_KEYS[#PRESET_KEYS+1]=k; seen[k]=true end end
    for _,k in ipairs(POSE_KEYS)  do if not seen[k] then PRESET_KEYS[#PRESET_KEYS+1]=k; seen[k]=true end end
  end

  local function listPresetTxtFiles(dir)
    local files, err = app.fs.listFiles(dir)
    if not files then return {}, "Failed to list files: " .. (err or "unknown") end

    local out = {}
    local seen = {}
    for _, file in ipairs(files) do
      local lower = string.lower(file)
      if lower:match("^.+%.txt$") then
        if not lower:match("%.shape%.txt$") and not lower:match("%.pose%.txt$") and not lower:match("%.sphere%.txt$") then
          if not seen[lower] then
            seen[lower] = true
            table.insert(out, file)
          end
        end
      end
    end

    table.sort(out, function(a,b) return string.lower(a) < string.lower(b) end)
    return out, nil
  end

  local dlg -- forward declare
  local CURRENT_PRESET_FILE = "default.txt" -- authoritative selection (do NOT trust dlg.data timing)

  local function fileSafe(s)
    s = tostring(s or "")
    s = s:gsub("[\\/]+", "")
    s = s:gsub("[^%w%._%-%s]", "")
    s = s:gsub("^%s+", ""):gsub("%s+$", "")
    if s == "" then s = "default.txt" end
    if not string.lower(s):match("%.txt$") then s = s .. ".txt" end
    return s
  end

  local function selectedFileSafe()
    return fileSafe(CURRENT_PRESET_FILE)
  end

  local function ensureDefaultFiles()
    local path = presetPathForSelected("default.txt")
    if not app.fs.isFile(path) then
      saveKeyValueFile(path, {
        cut_depth=13,

        jaw_w=0,
        jaw_h=25,
        jaw_d=0,

        chin_w=13,
        chin_h=25,
        chin_d=92,

        -- nose:
        --   nose_br = TOP Y (0..r, % of r from top center down)
        --   nose_bh = BOTTOM offset DOWN from TOP (additive, % of r)
        --   nose_th = TIP offset DOWN from TOP (additive, % of r; clamped within top..bottom)
        nose_bh=20,
        nose_br=50,
        nose_th=60,
        nose_td=40,

        show_outer=true,
        show_cross=true,
        show_cuts=true,
        show_face=true,
        show_jaws=true,
        show_chin=true,
        show_nose=true,
        show_ears=true,

        ear_back=14,
        ear_rot=0,
        ear_x=0,
        ear_scale=100,
        ear_y=0,
        ear_z=0,

        alpha_front=255,
        alpha_back=26,

        -- group enables (shape)
        jawchin_en=true,
        nose_en=true,
        ears_en=true,

        p1x=0.5, p1y=0.5,
        roll=0,
        radius_t=50,
        radius_px=0,
        roll_side="", -- none selected => center
        full_180=false,
      }, PRESET_KEYS)
    end
  end

  ensureDefaultFiles()

  local BASE_OPTIONS, BO_ERR = listPresetTxtFiles(SCRIPT_DIR)
  if BO_ERR then app.alert(BO_ERR) end
  if #BASE_OPTIONS == 0 then BASE_OPTIONS = {"default.txt"} end
  do
    local hasDefault = false
    for _, f in ipairs(BASE_OPTIONS) do
      if string.lower(f) == "default.txt" then hasDefault = true break end
    end
    if not hasDefault then table.insert(BASE_OPTIONS, 1, "default.txt") end
  end

  local function mergeIntoPreset(path, patch)
    local existing = {}
    if app.fs.isFile(path) then
      local t, err = loadKeyValueFile(path)
      if not err and t then existing = t end
    end
    for k, v in pairs(patch) do existing[k] = v end
    local ok, err = saveKeyValueFile(path, existing, PRESET_KEYS)
    return ok, err
  end

  -- =========================================
  -- Sphere drawing
  -- =========================================
  local function cutFracFromDepth(cutDepthPct)
    local d = clamp(cutDepthPct or 13, 0, 95)
    local frac = 1.0 - (d / 100.0)
    return clamp(frac, 0.05, 0.98)
  end

  local function drawSphere(img, cx, cy, r, rotXdeg, rotYdeg, rotZdeg, fgColor,
                            roll_side,
                            cutDepthPct,
                            chinW_pct, chinH_pct, chinD_pct,
                            jawH_pct, jawW_pct, jawD_pct,
                            noseBH_pct, noseBR_pct, noseTH_pct, noseTD_pct,
                            show_outer, show_cross, show_cuts,
                            show_face, show_jaws, show_chin, show_nose, show_ears,
                            ear_back_deg, ear_rot_deg, ear_x_pct, ear_scale_pct, ear_y_pct, ear_z_pct,
                            alpha_front, alpha_back)

    local STEPS = math.max(96, math.floor(r * 8))

    local rx = deg2rad(rotXdeg)
    local ry = deg2rad(rotYdeg)
    local rz = deg2rad(rotZdeg)
    local cosX, sinX = math.cos(rx), math.sin(rx)
    local cosY, sinY = math.cos(ry), math.sin(ry)
    local cosZ, sinZ = math.cos(rz), math.sin(rz)

    local function rotZ(x, y, z)
      local x2 = x * cosZ - y * sinZ
      local y2 = x * sinZ + y * cosZ
      return x2, y2, z
    end
    local function rotY(x, y, z)
      local x2 = x * cosY + z * sinY
      local z2 = -x * sinY + z * cosY
      return x2, y, z2
    end
    local function rotX(x, y, z)
      local y2 = y * cosX - z * sinX
      local z2 = y * sinX + z * cosX
      return x, y2, z2
    end

    local function pivotForSide(side)
      side = tostring(side or ""):lower()
      if side == "top" then
        return 0, -r, 0
      elseif side == "left" then
        return -r, 0, 0
      elseif side == "right" then
        return r, 0, 0
      elseif side == "bottom" then
        return 0, r, 0
      end
      return 0, 0, 0
    end

    local pivx, pivy, pivz = pivotForSide(roll_side)

    local function proj(x, y) return cx + x, cy + y end

    local sr, sg, sb, sa = fgColor.red, fgColor.green, fgColor.blue, fgColor.alpha

    alpha_front = clamp(tonumber(alpha_front) or 255, 0, 255)
    alpha_back  = clamp(tonumber(alpha_back)  or 26,  0, 255)

    local frontA = math.floor(sa * (alpha_front / 255) + 0.5)
    local backA  = math.floor(sa * (alpha_back  / 255) + 0.5)

    local wirePix = app.pixelColor.rgba(sr, sg, sb, frontA)

    local backEnabled = (backA > 0)
    local backPix = backEnabled and app.pixelColor.rgba(sr, sg, sb, backA) or nil

    local function xform3D_pivot(x, y, z)
      x = x - pivx; y = y - pivy; z = z - pivz
      x, y, z = rotZ(x, y, z)
      x, y, z = rotY(x, y, z)
      x, y, z = rotX(x, y, z)
      x = x + pivx; y = y + pivy; z = z + pivz
      local px, py = proj(x, y)
      return px, py, z
    end

    local function xform3D_noYaw_pivot(x, y, z)
      x = x - pivx; y = y - pivy; z = z - pivz
      x, y, z = rotZ(x, y, z)
      x, y, z = rotX(x, y, z)
      x = x + pivx; y = y + pivy; z = z + pivz
      local px, py = proj(x, y)
      return px, py, z
    end

    local function pixForZ(zv)
      if zv < 0 then
        return backEnabled and backPix or nil
      end
      return wirePix
    end

    local function buildRingPoints(pointFunc)
      local pts = {}
      for i = 0, STEPS do
        local t = (i / STEPS) * (math.pi * 2)
        local x, y, z = pointFunc(t)
        local px, py, pz = xform3D_pivot(x, y, z)
        pts[#pts+1] = {x=px, y=py, z=pz}
      end
      return pts
    end

    local equatorPts = buildRingPoints(function(t)
      return r * math.cos(t), 0, r * math.sin(t)
    end)
    local meridianPts = buildRingPoints(function(t)
      return 0, r * math.sin(t), r * math.cos(t)
    end)

    local CUT_FRAC = cutFracFromDepth(cutDepthPct)
    local cutx = r * CUT_FRAC
    local cutRem = r * math.sqrt(math.max(0, 1 - CUT_FRAC*CUT_FRAC))

    local cutLeftPts = buildRingPoints(function(t)
      return -cutx, cutRem * math.sin(t), cutRem * math.cos(t)
    end)
    local cutRightPts = buildRingPoints(function(t)
      return  cutx, cutRem * math.sin(t), cutRem * math.cos(t)
    end)
    local cutTopPts = buildRingPoints(function(t)
      return cutRem * math.sin(t), -cutx, cutRem * math.cos(t)
    end)
    local cutBottomPts = buildRingPoints(function(t)
      return cutRem * math.sin(t),  cutx, cutRem * math.cos(t)
    end)

    local segs = {}
    local function addSegmentsFrom(pts, kind)
      for i = 1, #pts - 1 do
        local a = pts[i]
        local b = pts[i+1]
        segs[#segs+1] = { x0=a.x, y0=a.y, x1=b.x, y1=b.y, z=(a.z+b.z)*0.5, kind=kind }
      end
    end

    addSegmentsFrom(equatorPts, 0)
    addSegmentsFrom(meridianPts, 0)
    addSegmentsFrom(cutLeftPts,  1)
    addSegmentsFrom(cutRightPts, 1)
    addSegmentsFrom(cutTopPts,   1)
    addSegmentsFrom(cutBottomPts,1)

    table.sort(segs, function(a, b) return a.z < b.z end)

    if show_cross then
      for _, s in ipairs(segs) do
        if s.kind == 0 then
          local pix = pixForZ(s.z)
          if pix then drawLineSet(img, s.x0, s.y0, s.x1, s.y1, pix) end
        end
      end
    end

    if show_cuts then
      for _, s in ipairs(segs) do
        if s.kind == 1 then
          local pix = pixForZ(s.z)
          if pix then drawLineSet(img, s.x0, s.y0, s.x1, s.y1, pix) end
        end
      end
    end

    -- Jaw/chin
    do
      local cw = clamp((chinW_pct or 13), 0, 100) / 100.0 * r
      local cd = clamp((chinD_pct or 92), 0, 100) / 100.0 * r

      local jh = clamp((jawH_pct  or 25), 0, 200) / 100.0 * r
      local chinRelH = clamp((chinH_pct or 25), 0, 100) / 100.0 * r

      local jw = clamp((jawW_pct or 0), 0, 100) / 100.0
      local jawX = cutx * (1.0 - jw)

      local jd = clamp((jawD_pct or 0), 0, 100) / 100.0 * r

      local jawBaseY = cutRem + jh
      local chinY = jawBaseY + chinRelH

      local chinLx, chinLy, chinLz = -cw, chinY, cd
      local chinRx, chinRy, chinRz =  cw, chinY, cd

      local lf_x, lf_y, lf_z = -cutx, 0, cutRem
      local rf_x, rf_y, rf_z =  cutx, 0, cutRem

      local lb_x, lb_y, lb_z = -cutx, cutRem, 0
      local rb_x, rb_y, rb_z =  cutx, cutRem, 0

      local lv_x, lv_y, lv_z = -jawX, jawBaseY, jd
      local rv_x, rv_y, rv_z =  jawX, jawBaseY, jd

      local lfpx, lfpy, lfpz = xform3D_pivot(lf_x, lf_y, lf_z)
      local rfpx, rfpy, rfpz = xform3D_pivot(rf_x, rf_y, rf_z)

      local lbpx, lbpy, lbpz = xform3D_pivot(lb_x, lb_y, lb_z)
      local rbpx, rbpy, rbpz = xform3D_pivot(rb_x, rb_y, rb_z)

      local lvpx, lvpy, lvpz = xform3D_pivot(lv_x, lv_y, lv_z)
      local rvpx, rvpy, rvpz = xform3D_pivot(rv_x, rv_y, rv_z)

      local chinLpx, chinLpy, chinLpz = xform3D_pivot(chinLx, chinLy, chinLz)
      local chinRpx, chinRpy, chinRpz = xform3D_pivot(chinRx, chinRy, chinRz)

      if show_face then
        local p = pixForZ((lfpz+chinLpz)*0.5); if p then drawLineSet(img, lfpx, lfpy, chinLpx, chinLpy, p) end
        p = pixForZ((rfpz+chinRpz)*0.5); if p then drawLineSet(img, rfpx, rfpy, chinRpx, chinRpy, p) end
      end

      if show_jaws then
        local p = pixForZ((lbpz+lvpz)*0.5); if p then drawLineSet(img, lbpx, lbpy, lvpx, lvpy, p) end
        p = pixForZ((rbpz+rvpz)*0.5); if p then drawLineSet(img, rbpx, rbpy, rvpx, rvpy, p) end
        p = pixForZ((lvpz+chinLpz)*0.5); if p then drawLineSet(img, lvpx, lvpy, chinLpx, chinLpy, p) end
        p = pixForZ((rvpz+chinRpz)*0.5); if p then drawLineSet(img, rvpx, rvpy, chinRpx, chinRpy, p) end
      end

      if show_chin then
        local p = pixForZ((chinLpz+chinRpz)*0.5); if p then drawLineSet(img, chinLpx, chinLpy, chinRpx, chinRpy, p) end
      end
    end

    -- Nose (top base; bottom/tip additive)
    if show_nose then
      local topPct = clamp((noseBR_pct or 50), 0, 100) / 100.0
      local topY = topPct * r

      local bottomOff = clamp((noseBH_pct or 20), 0, 100) / 100.0
      local bottomY = clamp(topY + (bottomOff * r), topY, r)

      local tipOff = clamp((noseTH_pct or 60), 0, 100) / 100.0
      local tipY = clamp(topY + (tipOff * r), topY, bottomY)

      local td = clamp((noseTD_pct or 40), 0, 100) / 100.0

      local faceY = 0
      local faceZ = r

      local CUT_FRAC2 = cutFracFromDepth(cutDepthPct)
      local cutRem2 = r * math.sqrt(math.max(0, 1 - CUT_FRAC2*CUT_FRAC2))

      local jh = clamp((jawH_pct  or 25), 0, 200) / 100.0 * r
      local chinRelH = clamp((chinH_pct or 25), 0, 100) / 100.0 * r
      local chinY = (cutRem2 + jh) + chinRelH

      local cd = clamp((chinD_pct or 92), 0, 100) / 100.0 * r
      local chinZ = cd

      local function depthFromTopToChinLine(yv)
        local denom = (chinY - faceY)
        if math.abs(denom) < 0.00001 then return faceZ end
        local t = (yv - faceY) / denom
        t = clamp(t, -0.25, 1.25)
        return faceZ + t * (chinZ - faceZ)
      end

      local topZ     = depthFromTopToChinLine(topY)
      local bottomZ  = depthFromTopToChinLine(bottomY)
      local tipBaseZ = depthFromTopToChinLine(tipY)
      local tipZ = tipBaseZ + (td * r)

      local mx, my, mz = xform3D_pivot(0, topY,    topZ)
      local bx, by, bz = xform3D_pivot(0, bottomY, bottomZ)
      local tx, ty, tz = xform3D_pivot(0, tipY,    tipZ)

      local p = pixForZ((mz+bz)*0.5); if p then drawLineSet(img, mx, my, bx, by, p) end
      p = pixForZ((mz+tz)*0.5); if p then drawLineSet(img, mx, my, tx, ty, p) end
      p = pixForZ((tz+bz)*0.5); if p then drawLineSet(img, tx, ty, bx, by, p) end
    end

    -- Ears
    if show_ears then
      local CUT_FRAC = cutFracFromDepth(cutDepthPct)
      local cutx = r * CUT_FRAC
      local cutRem = r * math.sqrt(math.max(0, 1 - CUT_FRAC*CUT_FRAC))

      local sc = clamp((ear_scale_pct or 100), 10, 300) / 100.0
      local earRx = r * 0.22 * sc
      local earRy = r * 0.30 * sc

      local earBack = clamp((ear_back_deg or 14), -60, 60)
      local earRotExtra = clamp((ear_rot_deg or 0), -180, 180) - 180

      local earMoveX = clamp((ear_x_pct or 0), -100, 100) / 100.0 * r
      local earMoveY = clamp((ear_y_pct or 0), -100, 100) / 100.0 * r
      local earZoff  = clamp((ear_z_pct or 0), -100, 100) / 100.0 * r

      local earCY = (cutRem - earRy)
      local stepsEar = math.max(64, math.floor(r * 6))

      local function computeEarBaseRotZ_noYaw()
        local side = 1
        local A_x, A_y = xform3D_noYaw_pivot(side * cutx, cutRem, 0)
        local dy = math.max(1, math.floor(r * 0.10 + 0.5))
        local y2 = clamp(cutRem - dy, -cutRem, cutRem)
        local B_x, B_y = xform3D_noYaw_pivot(side * cutx, y2, 0)
        local a2 = atan2(B_y - A_y, B_x - A_x)
        return a2 - (math.pi * 0.5)
      end

      local baseRotZ = computeEarBaseRotZ_noYaw()
      local rotZr = (baseRotZ + deg2rad(earRotExtra))
      local cz, sz = math.cos(rotZr), math.sin(rotZr)

      local by = deg2rad(earBack)
      local cyb, syb = math.cos(by), math.sin(by)

      local function rotZ2(x, y)
        local x2 = x * cz - y * sz
        local y2 = x * sz + y * cz
        return x2, y2
      end

      local function rotY3(x, y, z)
        local x2 = x * cyb + z * syb
        local z2 = -x * syb + z * cyb
        return x2, y, z2
      end

      local centerOff = earRx

      local function drawEar(sideSign)
        local pivotX = cutx
        local pivotY = clamp(earCY, -cutRem, cutRem)
        local pivotZ = 0 + earZoff

        local prev = nil
        for i = 0, stepsEar do
          local t = (i / stepsEar) * (math.pi * 2)

          local ex = centerOff + earRx * math.cos(t)
          local ey = earRy * math.sin(t)
          local ez = 0

          ex, ey = rotZ2(ex, ey)
          ex, ey, ez = rotY3(ex, ey, ez)

          local wx = pivotX + ex
          local wy = pivotY + ey
          local wz = pivotZ + ez

          if sideSign < 0 then wx = -wx end

          wx = wx + (earMoveX * sideSign)
          wy = wy + earMoveY

          local px, py, pz = xform3D_pivot(wx, wy, wz)

          if prev then
            local p = pixForZ((prev.z+pz)*0.5)
            if p then drawLineSet(img, prev.x, prev.y, px, py, p) end
          end
          prev = {x=px, y=py, z=pz}
        end
      end

      drawEar(-1)
      drawEar( 1)
    end

    -- Outer outline follows pivot-selected transform
    if show_outer then
      local ocx, ocy = xform3D_pivot(0, 0, 0)
      local prevx, prevy
      for i = 0, STEPS do
        local t = (i / STEPS) * (math.pi * 2)
        local sx = r * math.cos(t)
        local sy = r * math.sin(t)
        local px = ocx + sx
        local py = ocy + sy
        if i > 0 then drawLineSet(img, prevx, prevy, px, py, wirePix) end
        prevx, prevy = px, py
      end
    end

    return frontA
  end

  -- =========================
  -- Radius base (selection aware)
  -- =========================
  local spriteMaxR = math.max(2, math.floor(math.min(spr.width, spr.height)/2) - 1)
  local b0 = getSelectionBounds()
  local selR0 = selectionMaxRadius(b0)
  local r0 = clamp(selR0 or math.max(6, math.floor(math.min(spr.width, spr.height)/4)), 2, spriteMaxR)

  local PAD_W, PAD_H = 72, 72
  local margin = 6

  local drag1 = false
  local p1x, p1y = 0.5, 0.5

  local function screenToN(x, y)
    local nx = (x - margin) / (PAD_W - margin*2)
    local ny = (y - margin) / (PAD_H - margin*2)
    return clamp(nx, 0, 1), clamp(ny, 0, 1)
  end

  local function pad1ToYawPitch(nx, ny, full180)
    if full180 then
      local yaw   = (nx - 0.5) * 360
      local pitch = (0.5 - ny) * 360
      return clamp(yaw,   -180, 180),
             clamp(pitch, -180, 180)
    else
      local yaw   = (nx - 0.5) * 180
      local pitch = (0.5 - ny) * 180
      return clamp(yaw,   -89, 89),
             clamp(pitch, -89, 89)
    end
  end

  local function radiusFromSliderT(t, baseR)
    baseR = clamp(math.floor(baseR + 0.5), 2, spriteMaxR)
    local maxR = clamp(baseR * 10, 2, spriteMaxR)
    t = clamp(t or 50, 0, 100)
    if t <= 50 then
      local u = t / 50
      return clamp(math.floor(lerp(2, baseR, u) + 0.5), 2, spriteMaxR)
    else
      local u = (t - 50) / 50
      return clamp(math.floor(lerp(baseR, maxR, u) + 0.5), 2, spriteMaxR)
    end
  end

  local function sliderTFromRadiusPx(rWanted, baseR)
    baseR = clamp(math.floor(baseR + 0.5), 2, spriteMaxR)
    local maxR = clamp(baseR * 10, 2, spriteMaxR)
    rWanted = clamp(math.floor((rWanted or baseR) + 0.5), 2, spriteMaxR)

    if rWanted <= baseR then
      local denom = math.max(1, baseR - 2)
      local u = (rWanted - 2) / denom
      return clamp(u * 50, 0, 50)
    else
      local denom = math.max(1, maxR - baseR)
      local u = (rWanted - baseR) / denom
      return clamp(50 + u * 50, 50, 100)
    end
  end

  -- =========================
  -- Grouped UI helpers
  -- =========================
  local lastBaseR = -1
  local loading = false
  local wireAlphaForPad = 255

  local GROUP_WIDGETS = {}
  local function reg(g, id)
    if not GROUP_WIDGETS[g] then GROUP_WIDGETS[g] = {} end
    table.insert(GROUP_WIDGETS[g], id)
  end

  local function setGroupState(g)
    if not dlg then return end
    local en   = (dlg.data[g .. "_en"] == true)
    local show = (dlg.data[g .. "_show"] == true)
    local ids  = GROUP_WIDGETS[g] or {}
    for _, id in ipairs(ids) do
      pcall(function()
        dlg:modify { id = id, visible = show, enabled = en }
      end)
    end
  end

  function updatePreview() end -- forward declare

  local function groupHeader(g, labelText)
    dlg:check {
      id = g .. "_en",
      label = labelText,
      text = "Enable",
      selected = true,
      onclick = function()
        setGroupState(g)
        updatePreview()
      end
    }
    dlg:check {
      id = g .. "_show",
      text = "Settings",
      selected = false,
      onclick = function()
        setGroupState(g)
        updatePreview()
      end
    }
    dlg:newrow()
  end

  -- Roll-side: exclusive when selecting one, but allows "none selected" (center pivot).
  local function rollSideChecks(labelText, ids)
    local updating = false

    local function uncheckAll()
      for _, id in ipairs(ids) do
        dlg:modify { id = id, selected = false }
      end
    end

    local function makeOnclick(id)
      return function()
        if updating then return end
        updating = true

        local now = (dlg.data[id] == true)
        if now then
          for _, other in ipairs(ids) do
            if other ~= id then dlg:modify { id = other, selected = false } end
          end
          dlg:modify { id = id, selected = true }
        else
          dlg:modify { id = id, selected = false }
        end

        updating = false
        updatePreview()
      end
    end

    for i, id in ipairs(ids) do
      local txt = id:match("roll_side_(.+)") or id
      if i == 1 then
        dlg:check { id=id, label=labelText, text=txt, selected=false, onclick=makeOnclick(id) }
      else
        dlg:check { id=id, text=txt, selected=false, onclick=makeOnclick(id) }
      end
    end
    dlg:newrow()
    uncheckAll()
  end

  local function applyShapeToUI(t)
    loading = true

    local function setSlider(id, v)
      if v == nil then return end
      pcall(function() dlg:modify{ id=id, value=tonumber(v) or dlg.data[id] } end)
    end
    local function setCheck(id, v)
      if v == nil then return end
      local b = (v == true) or (v == "true") or (tonumber(v) == 1)
      pcall(function() dlg:modify{ id=id, selected=b } end)
    end

    setSlider("cut_depth", t.cut_depth)

    setSlider("jaw_w", t.jaw_w)
    setSlider("jaw_h", t.jaw_h)
    setSlider("jaw_d", t.jaw_d)

    setSlider("chin_w", t.chin_w)
    setSlider("chin_h", t.chin_h)
    setSlider("chin_d", t.chin_d)

    setSlider("nose_bh", t.nose_bh)
    setSlider("nose_br", t.nose_br)
    setSlider("nose_th", t.nose_th)
    setSlider("nose_td", t.nose_td)

    setCheck("show_outer", t.show_outer)
    setCheck("show_cross", t.show_cross)
    setCheck("show_cuts",  t.show_cuts)

    setCheck("show_face",  t.show_face)
    setCheck("show_jaws",  t.show_jaws)
    setCheck("show_chin",  t.show_chin)
    setCheck("show_nose",  t.show_nose)
    setCheck("show_ears",  t.show_ears)

    setSlider("ear_back",  t.ear_back)
    setSlider("ear_rot",   t.ear_rot)
    setSlider("ear_x",     t.ear_x)
    setSlider("ear_scale", t.ear_scale)
    setSlider("ear_y",     t.ear_y)
    setSlider("ear_z",     t.ear_z)

    setSlider("alpha_front", t.alpha_front)
    setSlider("alpha_back",  t.alpha_back)

    -- GROUP ENABLES (shape)
    setCheck("jawchin_en", t.jawchin_en)
    setCheck("nose_en",    t.nose_en)
    setCheck("ears_en",    t.ears_en)

    loading = false
  end

  local function applyPoseToUI(t, includeRadius)
    loading = true
    if t.p1x ~= nil then p1x = clamp(tonumber(t.p1x) or p1x, 0, 1) end
    if t.p1y ~= nil then p1y = clamp(tonumber(t.p1y) or p1y, 0, 1) end
    if t.roll ~= nil then pcall(function() dlg:modify{ id="roll", value=tonumber(t.roll) or dlg.data.roll } end) end

    local function setCheck(id, v)
      if v == nil then return end
      local b = (v == true) or (v == "true") or (tonumber(v) == 1)
      pcall(function() dlg:modify{ id=id, selected=b } end)
    end

    setCheck("full_180", t.full_180)

    local rs = tostring(t.roll_side or ""):lower()
    pcall(function()
      dlg:modify{ id="roll_side_bottom", selected = false }
      dlg:modify{ id="roll_side_top",    selected = false }
      dlg:modify{ id="roll_side_left",   selected = false }
      dlg:modify{ id="roll_side_right",  selected = false }
    end)

    if rs ~= "" and rs ~= "center" then
      local map = { bottom="roll_side_bottom", top="roll_side_top", left="roll_side_left", right="roll_side_right" }
      local pick = map[rs]
      if pick then
        pcall(function() dlg:modify{ id=pick, selected = true } end)
      end
    end

    if includeRadius then
      local b = getSelectionBounds()
      local baseR = selectionMaxRadius(b) or r0
      baseR = clamp(math.floor(baseR + 0.5), 2, spriteMaxR)

      local rp = tonumber(t.radius_px)
      if rp and rp > 0 then
        local newT = sliderTFromRadiusPx(rp, baseR)
        pcall(function() dlg:modify{ id="radius_t", value=newT } end)
      elseif t.radius_t ~= nil then
        pcall(function() dlg:modify{ id="radius_t", value=tonumber(t.radius_t) or dlg.data.radius_t } end)
      end
    end

    loading = false
  end

  local function saveShape()
    local filename = selectedFileSafe()
    local path = presetPathForSelected(filename)

    local patch = {
      cut_depth = dlg.data.cut_depth or 13,

      jaw_w = dlg.data.jaw_w or 0,
      jaw_h = dlg.data.jaw_h or 25,
      jaw_d = dlg.data.jaw_d or 0,

      chin_w = dlg.data.chin_w or 13,
      chin_h = dlg.data.chin_h or 25,
      chin_d = dlg.data.chin_d or 92,

      nose_bh = dlg.data.nose_bh or 20,
      nose_br = dlg.data.nose_br or 50,
      nose_th = dlg.data.nose_th or 60,
      nose_td = dlg.data.nose_td or 40,

      show_outer = (dlg.data.show_outer == true),
      show_cross = (dlg.data.show_cross == true),
      show_cuts  = (dlg.data.show_cuts  == true),

      show_face  = (dlg.data.show_face  == true),
      show_jaws  = (dlg.data.show_jaws  == true),
      show_chin  = (dlg.data.show_chin  == true),
      show_nose  = (dlg.data.show_nose  == true),
      show_ears  = (dlg.data.show_ears  == true),

      ear_back  = dlg.data.ear_back  or 14,
      ear_rot   = dlg.data.ear_rot   or 0,
      ear_x     = dlg.data.ear_x     or 0,
      ear_scale = dlg.data.ear_scale or 100,
      ear_y     = dlg.data.ear_y     or 0,
      ear_z     = dlg.data.ear_z     or 0,

      alpha_front = dlg.data.alpha_front or 255,
      alpha_back  = dlg.data.alpha_back  or 26,

      -- GROUP ENABLES (shape)
      jawchin_en = (dlg.data.jawchin_en == true),
      nose_en    = (dlg.data.nose_en == true),
      ears_en    = (dlg.data.ears_en == true),
    }

    local ok, err = mergeIntoPreset(path, patch)
    if not ok then app.alert(err or "Save shape failed.") end
  end

  local function loadShape()
    local filename = selectedFileSafe()
    local path = presetPathForSelected(filename)
    local t, err = loadKeyValueFile(path)
    if err then return app.alert(err) end
    applyShapeToUI(t)
    for g,_ in pairs(GROUP_WIDGETS) do setGroupState(g) end
    updatePreview()
  end

  local function savePose()
    local filename = selectedFileSafe()
    local path = presetPathForSelected(filename)

    local b = getSelectionBounds()
    local baseR = selectionMaxRadius(b) or r0
    baseR = clamp(math.floor(baseR + 0.5), 2, spriteMaxR)
    local rPx = radiusFromSliderT(dlg.data.radius_t or 50, baseR)

    local function rollSideString()
      if dlg.data.roll_side_bottom then return "bottom" end
      if dlg.data.roll_side_top then return "top" end
      if dlg.data.roll_side_left then return "left" end
      if dlg.data.roll_side_right then return "right" end
      return "" -- none selected => center
    end

    local patch = {
      p1x = p1x,
      p1y = p1y,
      roll = dlg.data.roll or 0,
      radius_t = dlg.data.radius_t or 50,
      radius_px = rPx,
      roll_side = rollSideString(),
      full_180 = (dlg.data.full_180 == true),
    }

    local ok, err = mergeIntoPreset(path, patch)
    if not ok then app.alert(err or "Save pose failed.") end
  end

  local function loadPose(includeRadius)
    local filename = selectedFileSafe()
    local path = presetPathForSelected(filename)
    local t, err = loadKeyValueFile(path)
    if err then return app.alert(err) end
    applyPoseToUI(t, includeRadius == true)
    updatePreview()
  end

  local function currentRollSide()
    if dlg.data.roll_side_bottom then return "bottom" end
    if dlg.data.roll_side_top then return "top" end
    if dlg.data.roll_side_left then return "left" end
    if dlg.data.roll_side_right then return "right" end
    return ""
  end

  function updatePreview()
    if not dlg or loading then
      if dlg then dlg:repaint() end
      return
    end

    local b = getSelectionBounds()
    local cx, cy = getCenterFromBounds(b)
    cx = clamp(cx, 0, spr.width-1)
    cy = clamp(cy, 0, spr.height-1)

    local baseR = selectionMaxRadius(b) or r0
    baseR = clamp(math.floor(baseR + 0.5), 2, spriteMaxR)

    if baseR ~= lastBaseR then
      lastBaseR = baseR
      if dlg.data.radius_t == nil then dlg:modify{ id="radius_t", value=50 } end
    end

    local r = radiusFromSliderT(dlg.data.radius_t or 50, baseR)

    local full180 = (dlg.data.full_180 == true)
    local yaw, pitch = pad1ToYawPitch(p1x, p1y, full180)

    local roll = dlg.data.roll or 0
    local col = app.fgColor

    local cutDepth = dlg.data.cut_depth or 13

    local jawW = dlg.data.jaw_w or 0
    local jawH = dlg.data.jaw_h or 25
    local jawD = dlg.data.jaw_d or 0

    local noseBH = dlg.data.nose_bh or 20
    local noseBR = dlg.data.nose_br or 50
    local noseTH = dlg.data.nose_th or 60
    local noseTD = dlg.data.nose_td or 40

    local chinW = dlg.data.chin_w or 13
    local chinH = dlg.data.chin_h or 25
    local chinD = dlg.data.chin_d or 92

    local show_outer = (dlg.data.show_outer == true)
    local show_cross = (dlg.data.show_cross == true)
    local show_cuts  = (dlg.data.show_cuts  == true)

    local en_jawchin = (dlg.data.jawchin_en == true)
    local en_nose    = (dlg.data.nose_en == true)
    local en_ears    = (dlg.data.ears_en == true)

    local show_face  = (dlg.data.show_face  == true) and en_jawchin
    local show_jaws  = (dlg.data.show_jaws  == true) and en_jawchin
    local show_chin  = (dlg.data.show_chin  == true) and en_jawchin
    local show_nose  = (dlg.data.show_nose  == true) and en_nose
    local show_ears  = (dlg.data.show_ears  == true) and en_ears

    local ear_back  = dlg.data.ear_back  or 14
    local ear_rot   = dlg.data.ear_rot   or 0
    local ear_x     = dlg.data.ear_x     or 0
    local ear_scale = dlg.data.ear_scale or 100
    local ear_y     = dlg.data.ear_y     or 0
    local ear_z     = dlg.data.ear_z     or 0

    local a_front = dlg.data.alpha_front or 255
    local a_back  = dlg.data.alpha_back  or 26

    local roll_side = currentRollSide()

    app.transaction(function()
      local previewLayer = ensureLayer(PREVIEW_NAME)
      local img = Image(spr.width, spr.height, spr.colorMode)
      img:clear()

      wireAlphaForPad =
        drawSphere(img, cx, cy, r, pitch, yaw, roll, col,
                   roll_side,
                   cutDepth,
                   chinW, chinH, chinD,
                   jawH, jawW, jawD,
                   noseBH, noseBR, noseTH, noseTD,
                   show_outer, show_cross, show_cuts,
                   show_face, show_jaws, show_chin, show_nose, show_ears,
                   ear_back, ear_rot, ear_x, ear_scale, ear_y, ear_z,
                   a_front, a_back) or wireAlphaForPad

      replaceCel(previewLayer, app.activeFrame, img, Point(0,0))
    end)

    dlg:repaint()
    app.refresh()
  end

  local function celFrameNumber(cel)
    if cel.frameNumber then return cel.frameNumber end
    if cel.frame and cel.frame.frameNumber then return cel.frame.frameNumber end
    if cel.frame and cel.frame.number then return cel.frame.number end
    return app.activeFrame.frameNumber
  end

  local function applyPreview()
    local previewLayer = findLayerByName(PREVIEW_NAME)
    if not previewLayer then return end

    app.transaction(function()
      local outLayer = ensureLayer(OUTPUT_NAME)

      for _, pc in ipairs(previewLayer.cels) do
        if pc and pc.image then
          local frnum = celFrameNumber(pc)

          local outImg = Image(spr.width, spr.height, spr.colorMode)
          outImg:clear()

          local oldOutCel = outLayer:cel(frnum)
          if oldOutCel and oldOutCel.image then
            outImg:drawImage(oldOutCel.image, oldOutCel.position)
          end
          outImg:drawImage(pc.image, pc.position)

          replaceCel(outLayer, frnum, outImg, Point(0,0))
        end
      end

      deletePreviewLayer()
      app.activeLayer = outLayer
    end)

    app.refresh()
  end

  -- =========================
  -- DIALOG (grouped layout)
  -- =========================
  dlg = Dialog("Sphere – All Rot from Bottom")

  dlg:combobox{
    id="base_file",
    label="File",
    options=BASE_OPTIONS,
    option="default.txt",
    onchange=function(ev)
      -- IMPORTANT: use the event option as the authoritative selection
      if ev and ev.option then
        CURRENT_PRESET_FILE = ev.option
      elseif dlg and dlg.data and dlg.data.base_file then
        CURRENT_PRESET_FILE = dlg.data.base_file
      else
        CURRENT_PRESET_FILE = "default.txt"
      end
      CURRENT_PRESET_FILE = fileSafe(CURRENT_PRESET_FILE)
      loadShape()
    end
  }

  dlg:newrow()
  dlg:button{ text="Save shape", onclick=function() saveShape() end }
  dlg:button{ text="Load shape", onclick=function() loadShape() end }

  dlg:newrow()
  dlg:button{ text="Save pose", onclick=function() savePose() end }
  dlg:button{ text="Load pose", onclick=function() loadPose(true) end }

  dlg:newrow()
  dlg:slider{ id="radius_t", label="Radius:", min=0, max=100, value=50, onchange=updatePreview }
  dlg:slider{ id="cut_depth", min=0, max=95, value=13, onchange=updatePreview }
  dlg:newrow()

  dlg:canvas{
    id="pad1",
    label="",
    width=72, height=72,
    onpaint=function(ev)
      local gc = ev.context
      gc.color = Color{r=240,g=240,b=240,a=255}
      gc:fillRect(Rectangle(0,0,72,72))
      gc.color = Color{r=40,g=40,b=40,a=255}
      gc:rect(Rectangle(0,0,71,71))

      local a = clamp(tonumber(wireAlphaForPad) or 255, 0, 255)
      gc.color = Color{r=100,g=100,b=100,a=a}
      gc:fillRect(Rectangle(36,0,1,72))
      gc:fillRect(Rectangle(0,36,72,1))

      local x = math.floor((6 + p1x * (72 - 12)) + 0.5)
      local y = math.floor((6 + p1y * (72 - 12)) + 0.5)
      gc.color = Color{r=0,g=120,b=255,a=255}
      gc:fillRect(Rectangle(x-3,y-3,7,7))
    end,
    onmousedown=function(ev)
      p1x, p1y = screenToN(ev.x, ev.y)
      drag1 = true
      updatePreview()
    end,
    onmousemove=function(ev)
      if not drag1 then return end
      p1x, p1y = screenToN(ev.x, ev.y)
      updatePreview()
    end,
    onmouseup=function() drag1 = false end
  }

  dlg:newrow()
  dlg:check{
    id="full_180",
    label="",
    text="Full 180",
    selected=false,
    onclick=updatePreview
  }

  dlg:slider{ id="roll", label="Roll:", min=-180, max=180, value=0, onchange=updatePreview }
  dlg:newrow()

  rollSideChecks(
    "Roll side:",
    { "roll_side_bottom", "roll_side_top", "roll_side_left", "roll_side_right" }
  )

  dlg:check{ id="show_outer", label="sphere", text="Outer", selected=true, onclick=updatePreview }
  dlg:check{ id="show_cross", text="Cross", selected=true, onclick=updatePreview }
  dlg:check{ id="show_cuts",  text="Cuts",  selected=true, onclick=updatePreview }
  dlg:newrow()

  -- hidden (kept for preset state)
  dlg:check{ id="show_face", label="", text="Face", selected=true, visible=false, onclick=updatePreview }
  dlg:check{ id="show_jaws", text="Jaws", selected=true, visible=false, onclick=updatePreview }
  dlg:check{ id="show_chin", text="Chin", selected=true, visible=false, onclick=updatePreview }
  dlg:check{ id="show_nose", label="", text="Nose", selected=true, visible=false, onclick=updatePreview }
  dlg:check{ id="show_ears", text="Ears", selected=true, visible=false, onclick=updatePreview }

  groupHeader("jawchin", "Jaw/Chin:")

  dlg:slider{ id="jaw_w", label="Jaw:", min=0, max=100, value=0, onchange=updatePreview } ; reg("jawchin","jaw_w")
  dlg:slider{ id="jaw_h", min=0, max=200, value=25, onchange=updatePreview }            ; reg("jawchin","jaw_h")
  dlg:newrow()

  dlg:slider{ id="chin_w", label="Chin:", min=0, max=100, value=13, onchange=updatePreview } ; reg("jawchin","chin_w")
  dlg:slider{ id="chin_h", min=0, max=100, value=25, onchange=updatePreview }               ; reg("jawchin","chin_h")
  dlg:newrow()

  dlg:slider{ id="jaw_d",  label="Depth:", min=0, max=100, value=0, onchange=updatePreview } ; reg("jawchin","jaw_d")
  dlg:slider{ id="chin_d",                min=0, max=100, value=92, onchange=updatePreview } ; reg("jawchin","chin_d")
  dlg:newrow()

  groupHeader("nose", "Nose:")

  dlg:slider{ id="nose_br", label="Top:", min=0, max=100, value=50, onchange=updatePreview } ; reg("nose","nose_br")
  dlg:slider{ id="nose_bh", label="Bottom:", min=0, max=100, value=20, onchange=updatePreview } ; reg("nose","nose_bh")
  dlg:newrow()

  dlg:slider{ id="nose_th", label="Tip:", min=0, max=100, value=60, onchange=updatePreview } ; reg("nose","nose_th")
  dlg:slider{ id="nose_td", min=0, max=100, value=40, onchange=updatePreview }               ; reg("nose","nose_td")
  dlg:newrow()

  groupHeader("ears", "Ears:")
  dlg:slider{ id="ear_back", label="Rotation:", min=-60, max=60, value=14, onchange=updatePreview } ; reg("ears","ear_back")
  dlg:slider{ id="ear_rot",  min=-180, max=180, value=0, onchange=updatePreview }                   ; reg("ears","ear_rot")
  dlg:newrow()
  dlg:slider{ id="ear_x", label="Pos:", min=-100, max=100, value=0, onchange=updatePreview } ; reg("ears","ear_x")
  dlg:slider{ id="ear_y", min=-100, max=100, value=0, onchange=updatePreview }               ; reg("ears","ear_y")
  dlg:slider{ id="ear_z", min=-100, max=100, value=0, onchange=updatePreview }               ; reg("ears","ear_z")
  dlg:newrow()
  dlg:slider{ id="ear_scale", label="Scale:", min=10, max=300, value=100, onchange=updatePreview } ; reg("ears","ear_scale")
  dlg:newrow()

  dlg:slider{ id="alpha_front", label="Alpha:", min=0, max=255, value=255, onchange=updatePreview }
  dlg:slider{ id="alpha_back",  label="Alpha:", min=0, max=255, value=26,  onchange=updatePreview }
  dlg:newrow()

  dlg:button{ text="Apply", onclick=function() applyPreview(); dlg:close() end }
  dlg:button{ text="Cancel", onclick=function()
    app.transaction(function() deletePreviewLayer() end)
    dlg:close()
    app.refresh()
  end}

  dlg:show{ wait=false }

  -- Initialize authoritative selection from the dialog AFTER show.
  do
    local v = (dlg.data and dlg.data.base_file) or "default.txt"
    CURRENT_PRESET_FILE = fileSafe(v)
  end

  for g,_ in pairs(GROUP_WIDGETS) do
    setGroupState(g)
  end

  loadShape()
  updatePreview()
end