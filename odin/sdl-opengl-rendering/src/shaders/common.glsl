#define IN(loc, type, name) layout(location = loc) in type name;
#define OUT(loc, type, name) layout(location = loc) out type name;

#ifdef VERT
#define VIN(loc, type, name) IN(loc, type, name);
#define VARYING(loc, type, name) OUT(loc, type, name);
#define FOUT(loc, type, name)
#endif

#ifdef FRAG
#define VIN(loc, type, name)
#define VARYING(loc, type, name) IN(loc, type, name);
#define FOUT(loc, type, name) OUT(loc, type, name);
#endif
