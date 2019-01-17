class GL{
    constructor(){
        this.id = GL.id++;
        this.color = "#0000ff";

        this.objects = [];
    }
    createObject(){
        const obj = new Object(arguments);

        this.objects.push(obj);
    }
    init(){
        const canvas = `<canvas height="300" width="400" id=${this.id}></canvas>`;
        $("body").append(canvas);

        this.canvas = $(`#${this.id}`)[0];
        this.ctx = this.canvas.getContext("webgl2");

        console.log(this.canvas);

        if (!this.ctx){ console.error("WebGL context is not available."); return null; }

        return this;
    }
    updateColor(){
        this.ctx.clearColor(this.color);

        return this;
    }
    setColor(color){
        this.color = color;
        this.ctx.updateColor();

        return this;
    }
    clear(){
        this.ctx.clear(
            this.ctx.COLOR_BUFFER_BIT |
            this.ctx.DEPTH_BUFFER_BIT
        );
        return this;
    }
}
GL.id = 0;

export {GL};
