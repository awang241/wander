import { shallowMount } from '@vue/test-utils'
import ShareActivity from "../../src/components/Activities/ShareActivity";

let wrapper;

beforeEach(() => {
    wrapper = shallowMount(ShareActivity, {
        propsData: {},
        mocks: {},
        stubs: {},
        methods: {},
    });
    wrapper.vm.warningToast = jest.fn();
});

afterEach(() => {
    wrapper.destroy();
});

describe('ShareActivity.vue', () => {
    test('More restrictive is false when making privacy less restrictive', () => {
        wrapper.vm.originalPrivacy = "private"
        wrapper.vm.privacy = "public"
        expect(wrapper.vm.isPrivacyMoreRestrictive()).toStrictEqual(false);
    });
    test('More restrictive is false when privacy has not changed', () => {
        wrapper.vm.originalPrivacy = "private"
        wrapper.vm.privacy = "private"
        expect(wrapper.vm.isPrivacyMoreRestrictive()).toStrictEqual(false);
    });

    test('More restrictive is true when privacy is changed from public to privae', () => {
        wrapper.vm.originalPrivacy = "public"
        wrapper.vm.privacy = "private"
        expect(wrapper.vm.isPrivacyMoreRestrictive()).toStrictEqual(true);
    });
});
