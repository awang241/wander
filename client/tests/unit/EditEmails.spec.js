import { shallowMount } from '@vue/test-utils'
import EditEmails from "../../src/components/Profile/EditProfile/EditEmails";

let wrapper;

beforeEach(() => {
    wrapper = shallowMount(EditEmails, {
        propsData: {
            "profile": {
                "primary_email": "primary@gmail.com",
                "additional_email": ["secondary@gmail.com"]
            }
        },
        mocks: {},
        stubs: {},
        methods: {},
    });
    wrapper.vm.warningToast = jest.fn();
});

afterEach(() => {
    wrapper.destroy();
});

describe('AddActivity.vue', () => {
    test('Can change an optional email to a primary email', () => {
        wrapper.vm.newPrimaryEmail = "secondary@gmail.com"
        wrapper.vm.changePrimaryEmail()
        expect(wrapper.vm.primaryEmail).toStrictEqual("secondary@gmail.com");
        expect(wrapper.vm.optionalEmails).toStrictEqual(["primary@gmail.com"]);
    });
    test('Can Add an optional email', () => {
        wrapper.vm.newEmail = "secondary2@gmail.com"
        wrapper.vm.addEmail()
        expect(wrapper.vm.optionalEmails).toStrictEqual(["secondary@gmail.com", "secondary2@gmail.com"]);
    });
    test('Cant Add an invalid email', () => {
        wrapper.vm.newEmail = "fakeemail"
        wrapper.vm.addEmail()
        expect(wrapper.vm.optionalEmails).not.toContain("fakeemail");
    });
    test('Cant add more than 5 emails total', () => {
        wrapper.vm.optionalEmails = ["email1@gmail.com", "email2@gmail.com", "email3@gmail.com", "email4@gmail.com"]
        wrapper.vm.primaryEmail = "email5@gmail.com"
        wrapper.vm.newEmail = "email6@gmail.com"
        wrapper.vm.addEmail()
        expect(wrapper.vm.optionalEmails).toStrictEqual(["email1@gmail.com", "email2@gmail.com", "email3@gmail.com", "email4@gmail.com"]);
    });
    test('Cant Add an email already in use', () => {
        wrapper.vm.newEmail = "primary@gmail.com"
        wrapper.vm.addEmail()
        expect(wrapper.vm.optionalEmails).toStrictEqual(["secondary@gmail.com"]);
    });
    test('Cant Add an email with a blank address', () => {
        wrapper.vm.newEmail = ""
        wrapper.vm.addEmail()
        expect(wrapper.vm.optionalEmails).toStrictEqual(["secondary@gmail.com"]);
    });
});
