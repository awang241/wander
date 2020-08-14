import { shallowMount, createLocalVue } from '@vue/test-utils'
import ViewActivity from "../../src/components/ViewActivity";
import VueRouter from 'vue-router'

const localVue = createLocalVue()
localVue.use(VueRouter)
const router = new VueRouter()

let wrapper;
let adminWrapper;

beforeEach(() => {
    wrapper = shallowMount(ViewActivity, {
            localVue,
            router,
            computed: {
                hasShareAndEditPermissions() {
                    return false
                }
            }
        }
    );
    adminWrapper = shallowMount(ViewActivity, {
            localVue,
            router,
            computed: {
                hasShareAndEditPermissions() {
                    return true
                }
            }
        }
    );


});

afterEach(() => {
    wrapper.destroy();
});

describe('ViewActivity.vue', () => {
    test('Share and edit buttons are visible when user had admin permissions', () => {
        expect(adminWrapper.find("#shareButton").exists()).toBe(true)
        expect(adminWrapper.find("#editButton").exists()).toBe(true)
    });
    test('Share and edit buttons not visible when user is not a follower of the activity', () => {
        expect(wrapper.find("#shareButton").exists()).toBe(false)
        expect(wrapper.find("#editButton").exists()).toBe(false)
    });
});
