import { passwordEncryption } from './passwordEncryption'

export interface LoginReponseBody {
    uid: number
    encryption: passwordEncryption
}
