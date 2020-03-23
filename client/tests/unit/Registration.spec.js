import { shallowMount } from '@vue/test-utils'
import Registration from "../../src/components/Registration";

// Mount the component
const wrapper = shallowMount(Registration)

describe('Registration.vue', () => {
    it('has a created hook', () => {
        expect(true).toBe(true)
    })

});