import { createFormModal } from '../../../common/dom/forms/helpers/createFormModal'
import {
    usernameValidatorList,
    emailValidatorList,
    passwordValidatorList
} from '../validators/authValidators'
import { updateAccount } from '../../helpers/updateAccount'
import { Account } from '../../network/types/Account'
import { DialogManager } from '../../../common/dom/dialogs/classes/DialogManager'

const dialogManager = new DialogManager()

export const SignupModal = createFormModal({
    title: 'Signup',
    description: `To create an account you need to provide an username, email and a password.`,
    url: 'auth/create',
    fields: [
        {
            name: 'name',
            type: 'text',
            validators: usernameValidatorList()
        },
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
    onSubmit: (data: Account) => {
        updateAccount(data)
        dialogManager.add({
            title: 'Email verification',
            message:
                'To unlock the full set of features offered by lunrabox, please verify your email',
            actions: [],
            onClose: () => {}
        })
    }
})
