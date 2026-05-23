#ifdef __VERT__
  #define SDL_TEXTURE_SPACE space0
  #define SDL_TEXTURE_GROUP 0
#endif
#ifdef __FRAG__
  #define SDL_TEXTURE_SPACE space2
  #define SDL_TEXTURE_GROUP 2
#endif

#define SDL_STORAGE(name, ty, loc) \
[[vk::binding(loc, SDL_TEXTURE_GROUP)]] \
StructuredBuffer<ty> name##_buf : register(t##loc, SDL_TEXTURE_SPACE);

#define SDL_TEXTURE(name, loc) \
[[vk::binding(loc, SDL_TEXTURE_GROUP)]] \
Sampler2D name : register(t##loc, SDL_TEXTURE_SPACE);
