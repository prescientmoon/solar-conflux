class ShaderCompiler{
    constructor(gl){
        this.ctx = gl.ctx;
    }
	createShader(gl,src,type){
		const shader = this.ctx.createShader(type);

		this.ctx.shaderSource(shader,src);
		this.ctx.compileShader(shader);

		//Get Error data if shader failed compiling
		if(!this.ctx.getShaderParameter(shader, this.ctx.COMPILE_STATUS)){
			console.error(`Error compiling shader: ${src}. Log: ${gl.getShaderInfoLog(shader)}`);
			this.ctx.deleteShader(shader);
			return null;
		}

		return shader;
	}

	createProgram(gl,vShader,fShader,doValidate){
		//Link shaders together
		const prog = gl.ctx.createProgram();

		this.ctx.attachShader(prog,vShader);
		this.ctx.attachShader(prog,fShader);
		this.ctx.linkProgram(prog);

		//Check if successful
		if(!this.ctx.getProgramParameter(prog, gl.LINK_STATUS)){
			console.error(`Error creating shader program. Log: ${gl.getProgramInfoLog(prog)}`);
			this.ctx.deleteProgram(prog);
            return null;
		}

		//Only do this for additional debugging.
		if(doValidate){
			this.ctx.validateProgram(prog);
			if(!this.ctx.getProgramParameter(prog,gl.VALIDATE_STATUS)){
                console.error(`Error validating shader program. Log: ${gl.getProgramInfoLog(prog)}`);
    			this.ctx.deleteProgram(prog);
                return null;
			}
		}

		//Can delete the shaders since the program has been made.
		this.ctx.detachShader(prog,vShader); //TODO, detaching might cause issues on some browsers, Might only need to delete.
		this.ctx.detachShader(prog,fShader);
		this.ctx.deleteShader(fShader);
		this.ctx.deleteShader(vShader);

		return prog;
	}
    async getShader(url){
        const response = await fetch(url);
        const obj = await response.json();
        const text = obj.text;
        console.log(text);
    }
}

export {ShaderCompiler};
