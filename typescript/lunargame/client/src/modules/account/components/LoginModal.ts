import { createFormModal } from '../../../common/dom/forms/helpers/createFormModal'
import {
    emailValidatorList,
    passwordValidatorList
} from '../validators/authValidators'
import { Account } from '../../network/types/Account'
import { updateAccount } from '../../helpers/updateAccount'

export const LoginModal = createFormModal({
    title: 'Login',
    description: `To subscribe to this website, please enter you r email address here. We will send updates occasionally.`,
    url: 'auth/login',
    fields: [
        {
            name: 'email',
            type: 'email',
            validators: emailValidatorList()
        },
        {
            name: 'password',
            type: 'password',
            validators: passwordValidatorList()
        }
    ],
    onSubmit: updateAccount
})
