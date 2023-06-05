using ModernGL, GeometryBasics, SIMDPatch

#region Uniform_Camera
struct Uniform_Camera
    V::Mat4f
    P::Mat4f
end
#endregion

#region Camera
export Camera
mutable struct Camera
    "model transform of the camera in world space."
    transform::Mat4f
    "field of view in degrees. (set to 0 for orthographic camera mode)"
    fov::Float32
    "xy represents the normalized viewport coordinates, z is the near/far plane."
    rect::Rect3f
    "reference to the uniform buffer location assigned to this camera."
    data::URef{Uniform_Camera}
end
#endregion

#region constructors
function Camera(buffer::UniformArray{Uniform_Camera}; transform=one(Mat4f), fov=90, viewport=Rect2f(Vec2f(0), Vec2f(1)), near=0, far=1000)
    rect = Rect3f(Vec3f(viewport.origin..., near), Vec3f(viewport.widths..., far - near))

    push!(buffer, Uniform_Camera(inv(transform), projection(rect, fov)))
    data = URef(buffer, length(buffer))
    
    return Camera(transform, fov, rect, data)
end
#endregion

#region getters
FOV_PERSPECTIVE_CUTOFF = 1f0
export isperspective
isperpective(camera::Camera) = camera.fov >= FOV_PERSPECTIVE_CUTOFF
isperspective(fov) = fov >= FOV_PERSPECTIVE_CUTOFF

export isorthographic
isorthographic(camera::Camera) = camera < FOV_PERSPECTIVE_CUTOFF
isorthographic(fov) = fov < FOV_PERSPECTIVE_CUTOFF
#endregion

#region helpers
function perspective(invaspect, fov, near, far)
    fov = inv(tan(0.5deg2rad(fov)))
    norm = inv(near - far)
    return Mat4f(
        invaspect * fov, 0,   0,                0,
        0,               fov, 0,                0,
        0,               0,   (near + far)norm, (2near * far)norm,
        0,               0,   -1,               0
    )
end
function orthographic(near, far)
    norm = inv(near - far)
    return Mat4f(
        1, 0, 0,     0,
        0, 1, 0,     0,
        0, 0, 2norm, (near + far)norm,
        0, 0, 0,     1
    )
end
function projection(rect::Rect3f, fov)
    origin = rect.origin
    widths = rect.widths
    near = origin[3]
    far = near + widths[3]
    if isperpective(fov)
        invaspect = widths[2] / widths[1]
        return perspective(invaspect, fov, near, far)
    else
        return orthographic(near, far)
    end
end
#endregion

#region transformations
export upload!
function upload!(camera::Camera)
    P = projection(camera.rect, camera.fov)
    V = inv(camera.transform)

    camera.data[!] = Uniform_Camera(V, P)

    return camera.data
end

export upload
function upload(camera::Camera)
    P = projection(camera.rect, camera.fov)
    V = inv(camera.transform)

    camera.data[] = Uniform_Camera(V, P)

    return camera.data
end
#endregion

export activate!
function activate!(container::Rect2i, camera::Camera)
    co, cw, ro, rw = simd(container.origin, container.widths, camera.rect.origin, camera.rect.widths)

    xy = round(co + cw * ro.xy)
    wh = round(cw * rw.xy)
    glViewport(xy..., wh...)

    return camera.data
end