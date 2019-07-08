import { AccountPublicData } from './AccountPublicData'

export interface Account extends AccountPublicData {
    uid: string
    verified: boolean
}
